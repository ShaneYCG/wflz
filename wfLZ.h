#pragma once
#ifndef WF_LZ_H
#define WF_LZ_H

#ifdef _MSC_VER
	#if _MSC_VER < 1300
	   typedef signed   char  int8_t;
	   typedef unsigned char  uint8_t;
	   typedef signed   short int16_t;
	   typedef unsigned short uint16_t;
	   typedef signed   int   int32_t;
	   typedef unsigned int   uint32_t;
	#else
	   typedef signed   __int8  int8_t;
	   typedef unsigned __int8  uint8_t;
	   typedef signed   __int16 int16_t;
	   typedef unsigned __int16 uint16_t;
	   typedef signed   __int32 int32_t;
	   typedef unsigned __int32 uint32_t;
	#endif
	typedef signed   __int64 int64_t;
	typedef unsigned __int64 uint64_t;
#else
#ifdef __cplusplus
extern "C" {
#endif
	#include <stdint.h>
#ifdef __cplusplus
};
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

//! wfLZ_GetMaxCompressedSize()
/*! Use this to figure out the maximum size for your compression buffer */
extern uint32_t wfLZ_GetMaxCompressedSize( const uint32_t inSize );

//! wfLZ_GetWorkMemSize()
/*! Returns the minimum size for workMem passed to wfLZ_CompressFast and wfLZ_Compress */
extern uint32_t wfLZ_GetWorkMemSize();

//! wfLZ_CompressFast()
/* Returns the size of the compressed data
* CompressFast greatly speeds up compression, but potentially reduces compression ratio
  (it takes advantage of a hash table to quickly find potential matches, although maybe not the best ones)
* swapEndian = 0, compression and decompression are carried out on processors of the same endianness
*/
uint32_t wfLZ_CompressFast( const uint8_t* const in, const uint32_t inSize, uint8_t* const out, const uint8_t* workMem, const uint32_t swapEndian );

//! wfLZ_Compress()
/*! Returns the size of the compressed data
* Can't handle inSize == 0
* TODO: restrict would be nice
*/
extern uint32_t wfLZ_Compress( const uint8_t* const in, const uint32_t inSize, uint8_t* const out, const uint8_t* workMem, const uint32_t swapEndian );

//! wfLZ_GetDecompressedSize()
/*! Returns 0 if the data does not appear to be valid WFLZ */
extern uint32_t wfLZ_GetDecompressedSize( const uint8_t* const in );

//! wfLZ_GetCompressedSize()
/*! Returns 0 if the data does not appear to be valid WFLZ */
extern uint32_t wfLZ_GetCompressedSize( const uint8_t* const in );

//! wfLZ_Decompress()
/*! Use wfLZ_GetDecompressedSize to allocate an output buffer of the correct size */
extern void wfLZ_Decompress( const uint8_t* const in, uint8_t* const out );

//! wfLZ_GetHeaderSize()
/*!
* Returns 0 if data appears invalid
*/
uint32_t wfLZ_GetHeaderSize( const uint8_t* const in );

//! Chunk-based Compression
/*!
Chunk compression is an easy way to parallelize decompression.  Input is broken into chunks that can be decompressed independently.
Compression ratio will suffer a little bit.
*/

//! wfLZ_GetMaxChunkCompressedSize()
extern uint32_t wfLZ_GetMaxChunkCompressedSize( const uint32_t inSize, const uint32_t blockSize );

//! wfLZ_ChunkCompress()
/*!
* blockSize must be a multiple of WFLZ_CHUNK_PAD
* useFastCompress = 0, use Compress() instead of CompressFast()
* TODO: Would be nice to have parallelized compression functions for this
*/
extern uint32_t wfLZ_ChunkCompress( uint8_t* in, const uint32_t inSize, const uint32_t blockSize, uint8_t* out, const uint8_t* workMem, const uint32_t swapEndian, const uint32_t useFastCompress );

//! wfLZ_GetNumChunks()
/*!
* Returns 0 if data appears invalid
*/
uint32_t wfLZ_GetNumChunks( const uint8_t* const in );

//! wfLZ_ChunkDecompressCallback()
/*!
* TODO: document how the fuck to use this
* TODO: const correctness would be nice
*/
void wfLZ_ChunkDecompressCallback( uint8_t* in, void( *chunkCallback )( void* ) );

//! wfLZ_ChunkDecompressLoop()
/*!
* TODO: document how the fuck to use this
* TODO: const correctness would be nice
*/
uint8_t* wfLZ_ChunkDecompressLoop( uint8_t* in, uint32_t** chunkDesc );

#ifdef __cplusplus
};
#endif

#endif // WF_LZ_H
