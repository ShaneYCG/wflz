#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../wfLZ.h"

#ifndef NULL
	#define NULL 0
#endif

void hashlittle2( 
	const void *key,       /* the key to hash */
	size_t      length,    /* length of the key */
	uint32_t   *pc,        /* IN: primary initval, OUT: primary hash */
	uint32_t   *pb         /* IN: secondary initval, OUT: secondary hash */
);

union Hash
{
	struct
	{
		uint32_t hash32[2];
	};
	uint64_t hash64;
};

uint64_t HashData( void* data, const size_t dataSize )
{
	uint64_t hash;
	*( ((uint32_t*)&hash) + 0 ) = 13;
	*( ((uint32_t*)&hash) + 1 ) = 37;
	hashlittle2( data, dataSize, (((uint32_t*)&hash)+0), (((uint32_t*)&hash)+1) );
	return hash;
}

int main( int argc, char** argv )
{
	const char* in = argc == 2 ? argv[1] : "C:\\dev\\wflz-trunk\\example\\testdata\\shantae-idle.tga";

	uint32_t hashCheck = 1;
	uint64_t hashValue, hashValue2;

	uint32_t compressedSize, uncompressedSize;
	uint8_t* uncompressed;
	uint8_t* workMem;
	uint8_t* compressed;
	
	printf( "\nwfLZ[%s]\n", in );
	{
		FILE* fh = fopen( in, "rb" );
		if( fh == NULL )
		{
			printf( "Error: Could not read input file '%s'.\n", in );
			return 0;
		}
		fseek( fh, 0, SEEK_END );
		uncompressedSize = (uint32_t)ftell( fh );
		uncompressed = (uint8_t*)malloc( uncompressedSize );
		if( uncompressed == NULL )
		{
			fclose( fh );
			printf( "Error: Allocation failed.\n" );
			return 0;
		}
		fseek( fh, 0, SEEK_SET );
		if( fread( uncompressed, 1, uncompressedSize, fh ) != uncompressedSize )
		{
			fclose( fh );
			printf( "Error: File read failed.\n" );
			return 0;
		}
		fclose( fh );
	}

	workMem = (uint8_t*)malloc( wfLZ_GetWorkMemSize() );
	if( workMem == NULL )
	{
		printf( "Error: Allocation failed.\n" );
		return 0;
	}

	compressed = (uint8_t*)malloc( wfLZ_GetMaxCompressedSize( uncompressedSize ) );

	hashValue = hashCheck ? HashData( uncompressed, uncompressedSize ) : 0 ;

	compressedSize = wfLZ_CompressFast( uncompressed, uncompressedSize, compressed, workMem, 0 );
	free( workMem );

	if( hashCheck ) { memset( uncompressed, 0, uncompressedSize ); }

	wfLZ_Decompress( compressed, uncompressed );
	free( compressed );

	hashValue2 = hashCheck ? HashData( uncompressed, uncompressedSize ) : 0 ;

	free( uncompressed );

	printf( "Compression Ratio: %.2f\n", ((float)compressedSize)/((float)uncompressedSize) );

	if( hashCheck && hashValue != hashValue2 )
	{
		printf( "Error: Hash check mismatch!\n" );
		return -1;
	}

	return 0;
}
