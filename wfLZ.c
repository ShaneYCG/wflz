#include "wfLZ.h"

//
// Config
//

// TODO: tuning docs! explanations here are good but give little idea how much each variable matters

// lowers the overhead imposed by compression meta-data a bit at the cost of significantly limiting the maximum match distance (beneficial if compressed data is <=4KB -- maybe make this automatic?)
//#define WFLZ_SHORT_WINDOW

// some of these variables are limited by the types they are stored in, be careful if tweaking
#ifdef WFLZ_SHORT_WINDOW

	// size of wfLZ_Block, can't sizeof cuz padding
	#define WFLZ_BLOCK_SIZE         3

	// no point in compressing anything smaller than the block describing it
	#define WFLZ_MIN_MATCH_LEN      ( WFLZ_BLOCK_SIZE + 1 )

	// capped by max value of wfLZ_Block::length, + WFLZ_MIN_MATCH_LEN is free, requiring no extra bits
	#define WFLZ_MAX_MATCH_LEN      ( 0x1fU-1 ) + WFLZ_MIN_MATCH_LEN

	// capped by max value of wfLZ_Block::dist
	// WFLZ_MAX_MATCH_DIST_FAST is the distance used by CompressFast, it can be useful to set WFLZ_MAX_MATCH_DIST lower than WFLZ_MAX_MATCH_DIST_FAST to speed up Compress()
	#define WFLZ_MAX_MATCH_DIST      0x7ffU
	#define WFLZ_MAX_MATCH_DIST_FAST 0x7ffU
#else
	#define WFLZ_BLOCK_SIZE          4
	#define WFLZ_MIN_MATCH_LEN       ( WFLZ_BLOCK_SIZE + 1 ) 
	#define WFLZ_MAX_MATCH_LEN       ( 0xffU-1 ) + WFLZ_MIN_MATCH_LEN
	#define WFLZ_MAX_MATCH_DIST      0xfffU
	#define WFLZ_MAX_MATCH_DIST_FAST 0xffffU
#endif

// capped by wfLZ_Block::numLiterals
// this is the maximum length of uncompressible data, if this limit is reached, another block must be emitted
// in practice, raising this helps ratio a very slight amount, but is not worth the cost of making our compression block bigger
#define WFLZ_MAX_SEQUENTIAL_LITERALS 0xffU

// number of bytes required = WFLZ_DICTSIZE*sizeof( wfLZ_DictEntry )
// raising this can increase compression ratio slightly, but has a huge impact on the amount of working memory required
// the default value (0xffffU) requires 256KB working memory
#define WFLZ_DICT_SIZE               0xfffffU

// when using ChunkCompress() each block will be aligned to this -- makes PS3 SPU transfer convenient
#define WFLZ_CHUNK_PAD               256

//
// End Config
//

// Thanks Daniel A. Newby (Corwinoid) for this bit
#define WFLZ_LOG2_8BIT( v )  ( 8 - 90/(((v)/4+14)|1) - 2/((v)/2+1) )
#define WFLZ_LOG2_16BIT( v ) ( 8*((v)>255) + WFLZ_LOG2_8BIT((v) >>8*((v)>255)) ) 
#define WFLZ_LOG2_32BIT( v ) ( 16*((v)>65535L) + WFLZ_LOG2_16BIT((v)*1L >>16*((v)>65535L)) )
#define WFLZ_HASH_SHIFT      ( 31 - WFLZ_LOG2_32BIT( WFLZ_DICT_SIZE ) )

#define WFLZ_HASHPTR( x )    (  ( *( (uint32_t*)x ) * 2654435761U )  >>  ( WFLZ_HASH_SHIFT )  )

typedef struct _wfLZ_Block
{
	#ifdef WFLZ_SHORT_WINDOW
		uint16_t dist:11;  // how far to backtrack to memcpy
		uint16_t length:5; // how much to memcpy ( +(WFLZ_MIN_MATCH_LEN-1) )
	#else
		uint16_t dist;
		uint8_t length;
	#endif
	uint8_t  numLiterals;  // how many literals are there until the next wfLZ_Block
} wfLZ_Block;

typedef struct _wfLZ_Header
{
	char     sig[4];         // this can be WFLZ for a single compressed block, or ZLFW for a block-compressed stream
	uint32_t compressedSize;
	uint32_t decompressedSize;
	wfLZ_Block firstBlock;
} wfLZ_Header;

typedef struct _wfLZ_HeaderChunked
{
	char     sig[4];
	uint32_t compressedSize;
	uint32_t decompressedSize;
	uint32_t numChunks;
} wfLZ_HeaderChunked;

typedef struct _wfLZ_ChunkDesc
{
	uint32_t	offset;
} wfLZ_ChunkDesc;

typedef struct _wfLZ_DictEntry
{
	const uint8_t* inPos;
} wfLZ_DictEntry;

uint32_t wfLZ_MemCmp( const uint8_t* a, const uint8_t* b, const uint32_t maxLen );
void wfLZ_MemCpy( uint8_t* dst, const uint8_t* src, const uint32_t size );
void wfLZ_MemSet( uint8_t* dst, const uint8_t value, const uint32_t size );
uint32_t wfLZ_RoundUp( const uint32_t value, const uint32_t base ) { return ( value + ( base - 1 ) ) & ~( base - 1 ); }
void wfLZ_EndianSwap16( uint16_t* data ) { *data = ( (*data & 0xFF00) >> 8 ) | ( (*data & 0x00FF) << 8 ); }
void wfLZ_EndianSwap32( uint32_t* data ) { *data = ( (*data & 0xFF000000) >> 24 ) | ( (*data & 0x00FF0000) >> 8 ) | ( (*data & 0x0000FF00) << 8 ) | ( (*data & 0x000000FF) << 24 ); }

#ifndef NULL
	#define NULL 0
#endif

//#define WFLZ_DBG	// writes compression and decompression logs -- if everything is working, these should match!

#ifdef WFLZ_DBG
	#include <stdio.h>
	#define WF_LZ_DBG_COMPRESS_FAST_INIT FILE* dbgFh = fopen( "c:/dev/compress-fast.txt", "wb" );
	#define WF_LZ_DBG_COMPRESS_INIT      FILE* dbgFh = fopen( "c:/dev/compress.txt", "wb" );
	#define WF_LZ_DBG_DECOMPRESS_INIT    FILE* dbgFh = fopen( "c:/dev/decompress.txt", "wb" );
	#define WF_LZ_DBG( ... )             fprintf( dbgFh, __VA_ARGS__ )
	#define WF_LZ_DBG_SHUTDOWN           fclose( dbgFh );
#else
	#define WF_LZ_DBG_COMPRESS_FAST_INIT
	#define WF_LZ_DBG_COMPRESS_INIT
	#define WF_LZ_DBG_DECOMPRESS_INIT
	#define WF_LZ_DBG( ... )
	#define WF_LZ_DBG_SHUTDOWN
#endif

//! wfLZ_GetMaxCompressedSize()

uint32_t wfLZ_GetMaxCompressedSize( const uint32_t inSize )
{
	return
		// header
		sizeof( wfLZ_Header )
		+
		// size of uncompressible data
		(inSize/WFLZ_MAX_SEQUENTIAL_LITERALS + 1) * (WFLZ_MAX_SEQUENTIAL_LITERALS+WFLZ_BLOCK_SIZE)
		+
		// terminating block
		WFLZ_BLOCK_SIZE;
}

//! wfLZ_GetWorkMemSize()

uint32_t wfLZ_GetWorkMemSize()
{
	return WFLZ_DICT_SIZE * sizeof( wfLZ_DictEntry );
}

//! wfLZ_CompressFast()

uint32_t wfLZ_CompressFast( const uint8_t* const in, const uint32_t inSize, uint8_t* const out, const uint8_t* workMem, const uint32_t swapEndian )
{
	wfLZ_Header header;
	wfLZ_Block* block = &header.firstBlock;
	uint8_t* dst = out + sizeof( wfLZ_Header );
	const uint8_t* src = in;
	uint32_t bytesLeft = inSize;
	uint32_t numLiterals;
	wfLZ_DictEntry* dict = dict = ( wfLZ_DictEntry* )workMem;

	#ifdef WFLZ_SHORT_WINDOW
		if( swapEndian != 0 ) { abort(); } // endian swapping stuffs not set up for bit fields
	#endif

	WF_LZ_DBG_COMPRESS_FAST_INIT

	WF_LZ_DBG( "wfLZ_CompressFast( %u )\n", inSize );

	// init header
	header.sig[0] = 'W';
	header.sig[1] = 'F';
	header.sig[2] = 'L';
	header.sig[3] = 'Z';
	header.compressedSize = WFLZ_MIN_MATCH_LEN;
	header.decompressedSize = inSize;

	// init dictionary
	wfLZ_MemSet( ( uint8_t* )dict, 0, sizeof( wfLZ_DictEntry ) * WFLZ_DICT_SIZE );

	// starting literal characters
	{
		const uint8_t* literalsEnd = src + ( WFLZ_MIN_MATCH_LEN > bytesLeft ? bytesLeft : WFLZ_MIN_MATCH_LEN ) ;
		for(
			;
			src != literalsEnd;
			++src, ++dst, --bytesLeft
		)
		{
			const uint32_t hash = WFLZ_HASHPTR( src );
			dict[ hash ].inPos = src;
			*dst = *src;
			WF_LZ_DBG( "  literal [0x%02X] [%c]\n", *src, *src );
		}
		numLiterals = src - in;
	}

	//
	{
		while( bytesLeft )
		{
			uint32_t hash = WFLZ_HASHPTR( src );
			const uint8_t* matchPos;
			const uint8_t* windowStart = src - WFLZ_MAX_MATCH_DIST_FAST;
			uint32_t matchLength = 0;
			const uint32_t maxMatchLen = WFLZ_MAX_MATCH_LEN > bytesLeft ? bytesLeft : WFLZ_MAX_MATCH_LEN ;
			if( hash == WFLZ_DICT_SIZE ) --hash;
			matchPos = dict[ hash ].inPos;
			dict[ hash ].inPos = src;

			// a match was found, figure ensure it really is a match (not a hash collision), and determine its length
			if( matchPos != NULL && matchPos >= windowStart )
			{
				matchLength = wfLZ_MemCmp( src, matchPos, maxMatchLen );
			}
			if( matchLength >= WFLZ_MIN_MATCH_LEN )
			{
				const uint32_t matchDist = src - matchPos;

				block->numLiterals = ( uint8_t )numLiterals;
				if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
				block = ( wfLZ_Block* )dst;
				bytesLeft -= matchLength;
				dst += WFLZ_BLOCK_SIZE;
				src += matchLength;
				block->dist = ( uint16_t )matchDist;
				block->length = ( uint8_t )( matchLength - WFLZ_MIN_MATCH_LEN + 1 );
				numLiterals = 0;

				WF_LZ_DBG( "  backtrack [%u] len [%u]\n", matchDist, matchLength );
				header.compressedSize += WFLZ_BLOCK_SIZE;
			}

			// output a literal byte: no entries for this position found, entry is too far away, entry was a hash collision, or the entry did not meet the minimum match length
			else
			{
				// if we've hit the max number of sequential literals, we need to output a compression block header
				if( numLiterals == WFLZ_MAX_SEQUENTIAL_LITERALS )
				{
					block->numLiterals = ( uint8_t )numLiterals;
					if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
					block = ( wfLZ_Block* )dst;
					dst += WFLZ_BLOCK_SIZE;
					block->dist = block->length = 0;
					numLiterals = 0;
					header.compressedSize += WFLZ_BLOCK_SIZE;
				}

				++numLiterals;
				--bytesLeft;
				WF_LZ_DBG( "  literal [0x%02X] [%c]\n", *src, *src );
				*dst++ = *src++;
				++header.compressedSize;
			}
		}
	}

	// append the 'end' block
	{
		block->numLiterals = ( uint8_t )numLiterals;
		if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
		block = ( wfLZ_Block* )dst;
		block->dist = block->length = block->numLiterals = 0;
		header.compressedSize += WFLZ_BLOCK_SIZE;
	}

	// save the header
	if( swapEndian != 0 )
	{
		wfLZ_EndianSwap32( &header.compressedSize );
		wfLZ_EndianSwap32( &header.decompressedSize );
	}
	*( ( wfLZ_Header* )out ) = header;

	WF_LZ_DBG_SHUTDOWN

	return header.compressedSize + sizeof( wfLZ_Header );
}

//! wfLZ_Compress()

uint32_t wfLZ_Compress( const uint8_t* const in, const uint32_t inSize, uint8_t* const out, const uint8_t* workMem, const uint32_t swapEndian )
{
	wfLZ_Header header;
	wfLZ_Block* block = &header.firstBlock;
	uint8_t* dst = out + sizeof( wfLZ_Header );
	const uint8_t* src = in;
	uint32_t bytesLeft = inSize;
	uint32_t numLiterals = 0;
	wfLZ_DictEntry* dict = dict = ( wfLZ_DictEntry* )workMem;

	#ifdef WFLZ_SHORT_WINDOW
		if( swapEndian != 0 ) { abort(); } // endian swapping stuffs not set up for bit fields
	#endif

	WF_LZ_DBG_COMPRESS_INIT

	WF_LZ_DBG( "wfLZ_Compress( %u )\n", inSize );

	// init header
	header.sig[0] = 'W';
	header.sig[1] = 'F';
	header.sig[2] = 'L';
	header.sig[3] = 'Z';
	header.compressedSize = 0;
	header.decompressedSize = inSize;

	// init dictionary
	wfLZ_MemSet( ( uint8_t* )dict, 0, sizeof( wfLZ_DictEntry ) * WFLZ_DICT_SIZE );

	// the first bytes are always literal
	{
		const uint8_t* literalsEnd;
		for(
			literalsEnd = src + ( WFLZ_MIN_MATCH_LEN > bytesLeft ? bytesLeft : WFLZ_MIN_MATCH_LEN ) ;
			src != literalsEnd ;
			++dst, ++src, --bytesLeft, ++header.compressedSize, ++numLiterals
		)
		{
			const uint32_t hash = WFLZ_HASHPTR( src );
			dict[ hash ].inPos = src;
			*dst = *src;
			WF_LZ_DBG( "  literal [0x%02X] [%c]\n", *src, *src );
		}
	}

	// iterate through input bytes
	while( bytesLeft )
	{
		const uint8_t* windowEnd = src - 1;
		const uint8_t* window = windowEnd;
		uint32_t       maxMatchLen;
		uint32_t       bestMatchDist = 0;
		uint32_t       bestMatchLen = 0;
		const uint8_t* windowStart;

		// check hash table for early-fail
		const uint8_t* hashPos;
		uint32_t hash = WFLZ_HASHPTR( src );
		if( hash == WFLZ_DICT_SIZE ) --hash;
		hashPos = dict[ hash ].inPos;
		dict[ hash ].inPos = src;

		//
		if( hashPos != NULL )
		{
			maxMatchLen = WFLZ_MAX_MATCH_LEN > bytesLeft ? bytesLeft : WFLZ_MAX_MATCH_LEN ;
			windowStart = src - WFLZ_MAX_MATCH_DIST;
			if( windowStart > hashPos ) window = hashPos;
			if( windowStart < in ) windowStart = in;

			// now that we have a search window established for our current position, search it for potential matches
			for( ; window >= windowStart; --window )
			{
				uint32_t matchLen = wfLZ_MemCmp( window, src, maxMatchLen );
				if( matchLen > bestMatchLen )
				{
					bestMatchLen = matchLen;
					bestMatchDist = src - window;
					if( matchLen == maxMatchLen )
					{
						break;
					}
				}
			}
		}

		// if a match was found, output the corresponding compression block header
		if( bestMatchLen > WFLZ_MIN_MATCH_LEN )
		{
			block->numLiterals = ( uint8_t )numLiterals;
			if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
			block = ( wfLZ_Block* )dst;
			bytesLeft -= bestMatchLen;
			dst += WFLZ_BLOCK_SIZE;
			src += bestMatchLen;
			block->dist = ( uint16_t )bestMatchDist;
			block->length = ( uint8_t )( bestMatchLen - WFLZ_MIN_MATCH_LEN + 1 );
			numLiterals = 0;
			WF_LZ_DBG( "  backtrack [%u] len [%u]\n", bestMatchDist, bestMatchLen );
			header.compressedSize += WFLZ_BLOCK_SIZE;
		}
		// otherwise, output a literal byte
		else
		{
			// if we've hit the max number of sequential literals, we need to output a compression block header
			if( numLiterals == WFLZ_MAX_SEQUENTIAL_LITERALS )
			{
				block->numLiterals = ( uint8_t )numLiterals;
				if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
				block = ( wfLZ_Block* )dst;
				dst += WFLZ_BLOCK_SIZE;
				block->dist = block->length = 0;
				numLiterals = 0;
				header.compressedSize += WFLZ_BLOCK_SIZE;
			}

			++numLiterals;
			--bytesLeft;
			WF_LZ_DBG( "  literal [0x%02X] [%c]\n", *src, *src );
			*dst++ = *src++;
			++header.compressedSize;
		}
	}

	// append the 'end' block
	{
		block->numLiterals = ( uint8_t )numLiterals;
		if( swapEndian != 0 ){ wfLZ_EndianSwap16( &block->dist ); }
		block = ( wfLZ_Block* )dst;
		block->dist = block->length = block->numLiterals = 0;
		header.compressedSize += WFLZ_BLOCK_SIZE;
	}

	// save the header
	if( swapEndian != 0 )
	{
		wfLZ_EndianSwap32( &header.compressedSize );
		wfLZ_EndianSwap32( &header.decompressedSize );
	}
	*( ( wfLZ_Header* )out ) = header;

	WF_LZ_DBG_SHUTDOWN

	return header.compressedSize + sizeof( wfLZ_Header );
}

//! wfLZ_GetDecompressedSize()

uint32_t wfLZ_GetDecompressedSize( const uint8_t* const in )
{
	wfLZ_Header* header = ( wfLZ_Header* )in;
	if(
		( header->sig[0] == 'W' && header->sig[1] == 'F' && header->sig[2] == 'L' && header->sig[3] == 'Z' )
		||
		( header->sig[0] == 'Z' && header->sig[1] == 'L' && header->sig[2] == 'F' && header->sig[3] == 'W' )
	)
	{
		return header->decompressedSize;
	}
	return 0;
}

//! wfLZ_GetCompressedSize()

uint32_t wfLZ_GetCompressedSize( const uint8_t* const in )
{
	wfLZ_Header* header = ( wfLZ_Header* )in;
	if(
		( header->sig[0] == 'W' && header->sig[1] == 'F' && header->sig[2] == 'L' && header->sig[3] == 'Z' )
		||
		( header->sig[0] == 'Z' && header->sig[1] == 'L' && header->sig[2] == 'F' && header->sig[3] == 'W' )
	)
	{
		return header->compressedSize + sizeof( wfLZ_Header );
	}
	return 0;
}

//! wfLZ_Decompress()

void wfLZ_Decompress( const uint8_t* const in, uint8_t* const out )
{
	wfLZ_Header* header = ( wfLZ_Header* )in;
	uint8_t* dst = out;
	const uint8_t* src = in + sizeof( wfLZ_Header );
	uint8_t numLiterals = header->firstBlock.numLiterals;
	wfLZ_Block* block;
	uint32_t dist, len;
	uint16_t tmp;

	WF_LZ_DBG_DECOMPRESS_INIT
	WF_LZ_DBG( "wfLZ_Decompress()\n" );

WF_LZ_LITERALS:
	--numLiterals;
	WF_LZ_DBG( "  literal [0x%02X] [%c]\n", *src, *src );
	*dst++ = *src++;
	if( numLiterals ) goto WF_LZ_LITERALS;

WF_LZ_BLOCK:
	block = ( wfLZ_Block* )src;
	numLiterals = block->numLiterals;
	//dist = ( uint32_t )block->dist;
	( (uint8_t*)&tmp )[ 0 ] = ( (uint8_t*)&block->dist )[ 0 ]; // compensate for unaligned reads of a u16 (will work on x86, but some PPC variants are picky)
	( (uint8_t*)&tmp )[ 1 ] = ( (uint8_t*)&block->dist )[ 1 ];
	dist = ( uint32_t )tmp;
	len = ( uint32_t )block->length;

	if( len != 0 )
	{
		len += WFLZ_MIN_MATCH_LEN - 1;
		WF_LZ_DBG( "  backtrack [%u] len [%u]\n", block->dist, len );
		wfLZ_MemCpy( dst, dst - block->dist, len );
		dst += len;
	}
	src += WFLZ_BLOCK_SIZE;

	if( numLiterals == 0 )
	{
		if( dist == 0 && len == 0 )	// we've reached the end of the input
		{
			WF_LZ_DBG_SHUTDOWN
			return;
		}
		goto WF_LZ_BLOCK;
	}
	else
	{
		goto WF_LZ_LITERALS;
	}
}

//! wfLZ_GetHeaderSize()

uint32_t wfLZ_GetHeaderSize( const uint8_t* const in )
{
	if( in[0] == 'Z' && in[1] == 'L' && in[2] == 'F' && in[3] == 'W' )
	{
		const wfLZ_HeaderChunked* const header = ( const wfLZ_HeaderChunked* )in;
		return sizeof( wfLZ_HeaderChunked ) + sizeof( wfLZ_ChunkDesc )*header->numChunks;
	}
	if( in[0] == 'W' && in[1] == 'F' && in[2] == 'L' && in[3] == 'Z' )
	{
		return sizeof( wfLZ_Header );
	}
	return 0;
}

//! LZC_GetMaxChunkCompressedSize()

uint32_t wfLZ_GetMaxChunkCompressedSize( const uint32_t inSize, const uint32_t blockSize )
{
	const uint32_t numChunks = ( (inSize-1) / blockSize ) + 1;
	return ( wfLZ_GetMaxCompressedSize( blockSize )+sizeof( wfLZ_ChunkDesc ) ) * numChunks + sizeof( wfLZ_HeaderChunked );
}

//! LZC_ChunkCompress()
uint32_t wfLZ_ChunkCompress( uint8_t* in, const uint32_t inSize, const uint32_t blockSize, uint8_t* out, const uint8_t* workMem, const uint32_t swapEndian, const uint32_t useFastCompress )
{
	wfLZ_HeaderChunked* header;
	wfLZ_ChunkDesc* block;
	uint32_t bytesLeft;

	const uint32_t numChunks = ( (inSize-1) / blockSize ) + 1;
	uint32_t totalCompressedSize = 0;

	header = ( wfLZ_HeaderChunked* )out;
	out += sizeof( wfLZ_HeaderChunked );
	totalCompressedSize += sizeof( wfLZ_HeaderChunked );

	block = ( wfLZ_ChunkDesc* )out;
	out += sizeof( wfLZ_ChunkDesc ) * numChunks;
	totalCompressedSize += sizeof( wfLZ_ChunkDesc ) * numChunks;

	for( bytesLeft = inSize; bytesLeft != 0; /**/ )
	{
		const uint32_t decompressedSize = bytesLeft >= blockSize ? blockSize : bytesLeft ;
		const uint32_t compressedSize = wfLZ_RoundUp(
			useFastCompress == 0 ? wfLZ_Compress( in, decompressedSize, out, workMem, swapEndian ) : wfLZ_CompressFast( in, decompressedSize, out, workMem, swapEndian ),
			WFLZ_CHUNK_PAD
		);
		block->offset = totalCompressedSize;

		if( swapEndian != 0 )
		{
			wfLZ_EndianSwap32( &block->offset );
		}

		++block;
		bytesLeft           -= decompressedSize;
		in                  += decompressedSize;
		out                 += compressedSize;
		totalCompressedSize += compressedSize;
	}

	header->sig[0]           = 'Z';
	header->sig[1]           = 'L';
	header->sig[2]           = 'F';
	header->sig[3]           = 'W';
	header->decompressedSize = inSize;
	header->numChunks        = numChunks;
	header->compressedSize   = totalCompressedSize;
	if( swapEndian != 0 )
	{
		wfLZ_EndianSwap32( &header->decompressedSize );
		wfLZ_EndianSwap32( &header->compressedSize );
		wfLZ_EndianSwap32( &header->numChunks );
	}

	return totalCompressedSize;
}

//! wfLZ_GetNumChunks()

uint32_t wfLZ_GetNumChunks( const uint8_t* const in )
{
	const wfLZ_HeaderChunked* const header = ( const wfLZ_HeaderChunked* const )in;
	if( header->sig[0] == 'Z' && header->sig[1] == 'L' && header->sig[2] == 'F' && header->sig[3] == 'W' )
	{
		return header->numChunks;
	}
	return 0;
}

//! wfLZ_ChunkDecompressCallback()

void wfLZ_ChunkDecompressCallback( uint8_t* in, void( *chunkCallback )( void* ) )
{
	uint32_t chunkIdx;
	wfLZ_ChunkDesc* chunk;
	wfLZ_HeaderChunked* header = ( wfLZ_HeaderChunked* )in;
	const uint32_t numChunks = header->numChunks;
	in += sizeof( wfLZ_HeaderChunked );

	chunk = ( wfLZ_ChunkDesc* )in;
	in += sizeof( wfLZ_ChunkDesc ) * numChunks;

	for( chunkIdx = 0; chunkIdx != numChunks; ++chunkIdx )
	{
		chunkCallback( in );
		in += wfLZ_RoundUp( wfLZ_GetCompressedSize( in ), WFLZ_CHUNK_PAD );
	}
}

//! wfLZ_ChunkDecompressLoop()

uint8_t* wfLZ_ChunkDecompressLoop( uint8_t* in, uint32_t** chunkDesc )
{
	wfLZ_HeaderChunked* header = ( wfLZ_HeaderChunked* )in;
	wfLZ_ChunkDesc* chunks = ( wfLZ_ChunkDesc* )( in + sizeof( wfLZ_HeaderChunked ) );
	if( *chunkDesc == NULL )
	{
		*chunkDesc = ( uint32_t* )chunks;
	}
	else
	{
		++*chunkDesc;
		if( *chunkDesc == ( uint32_t* )chunks + header->numChunks ) { return NULL; }
	}
	return in + **chunkDesc;
}

/*!
Utility functions below, not exposed publicly
*/

//! wfLZ_MemCmp()
/*!
Deceptively named: the return value of this is *not* like strcmp/memcmp() -- it's just the number of sequential matching bytes, not some kind of diff
*/

#if 0
uint32_t wfLZ_MemCmp( const uint8_t* a, const uint8_t* b, const uint32_t maxLen )
{
	uint32_t matched = 0;
	while( *a++ == *b++ && matched < maxLen ) ++matched;
	return matched;
}
#else // this depends on unaligned access, but it is only called during Compress() which has other issues on CPUs that care
// Thanks Daniel A. Newby (Corwinoid) for optimizing these
uint32_t wfLZ_MemCmp_i( const uint32_t* a, const uint32_t* b, const uint32_t count, const uint32_t maxLen )
{
	if( count >= maxLen ) return maxLen;
	if( *a != *b )
	{
		uint32_t n = 0;
		uint8_t *i = (uint8_t*)a, *j = (uint8_t*)b;
		while( *i++ == *j++ && count + n < maxLen ) ++n;
		return count + n;
	}
	return wfLZ_MemCmp_i( ++a, ++b, count + 4, maxLen );
}
 
uint32_t wfLZ_MemCmp( const uint8_t* a, const uint8_t* b, const uint32_t maxLen )
{
	return wfLZ_MemCmp_i( (uint32_t*)a, (uint32_t*)b, 0, maxLen );
}
#endif

//! wfLZ_MemCpy()
/*!
DO NOT just try to replace this with standard memcpy! this must allow src and dst to overlap!
*/

void wfLZ_MemCpy( uint8_t* dst, const uint8_t* src, const uint32_t size )
{
	uint32_t i;
	for( i = 0; i != size; ++i ) *dst++ = *src++;
}

//! wfLZ_MemSet()
void wfLZ_MemSet( uint8_t* dst, const uint8_t value, const uint32_t size )
{
	uint32_t i;
	for( i = 0; i != size; ++i ) *dst++ = value;
}
