// crc.cc            see license.txt for copyright and terms of use
// adapted slightly by Scott McPeak

// originally was:
/* crc32h.c -- package to compute 32-bit CRC one byte at a time using   */
/*             the high-bit first (Big-Endian) bit ordering convention  */
/*                                                                      */
/* Synopsis:                                                            */
/*  gen_crc_table() -- generates a 256-word table containing all CRC    */
/*                     remainders for every possible 8-bit byte.  It    */
/*                     must be executed (once) before any CRC updates.  */
/*                                                                      */
/*  unsigned update_crc(crc_accum, data_blk_ptr, data_blk_size)         */
/*           unsigned crc_accum; char *data_blk_ptr; int data_blk_size; */
/*           Returns the updated value of the CRC accumulator after     */
/*           processing each byte in the addressed block of data.       */
/*                                                                      */
/*  It is assumed that an unsigned long is at least 32 bits wide and    */
/*  that the predefined type char occupies one 8-bit byte of storage.   */
/*                                                                      */
/*  The generator polynomial used for this version of the package is    */
/*  x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x^1+x^0 */
/*  as specified in the Autodin/Ethernet/ADCCP protocol standards.      */
/*  Other degree 32 polynomials may be substituted by re-defining the   */
/*  symbol POLYNOMIAL below.  Lower degree polynomials must first be    */
/*  multiplied by an appropriate power of x.  The representation used   */
/*  is that the coefficient of x^0 is stored in the LSB of the 32-bit   */
/*  word and the coefficient of x^31 is stored in the most significant  */
/*  bit.  The CRC is to be appended to the data most significant byte   */
/*  first.  For those protocols in which bytes are transmitted MSB      */
/*  first and in the same order as they are encountered in the block    */
/*  this convention results in the CRC remainder being transmitted with */
/*  the coefficient of x^31 first and with that of x^0 last (just as    */
/*  would be done by a hardware shift register mechanization).          */
/*                                                                      */
/*  The table lookup technique was adapted from the algorithm described */
/*  by Avram Perez, Byte-wise CRC Calculations, IEEE Micro 3, 40 (1983).*/

#define POLYNOMIAL 0x04c11db7L

static unsigned long crc_table[256];

void gen_crc_table()
 /* generate the table of CRC remainders for all possible bytes */
 { register int i, j;  register unsigned long crc_accum;
   for ( i = 0;  i < 256;  i++ )
       { crc_accum = ( (unsigned long) i << 24 );
         for ( j = 0;  j < 8;  j++ )
              { if ( crc_accum & 0x80000000L )
                   crc_accum =
                     ( crc_accum << 1 ) ^ POLYNOMIAL;
                else
                   crc_accum =
                     ( crc_accum << 1 ); }
         crc_table[i] = crc_accum; }
   return; }

unsigned long update_crc(unsigned long crc_accum, char const *data_blk_ptr,
                                                    int data_blk_size)
 /* update the CRC on the data block one byte at a time */
 { register int i, j;
   for ( j = 0;  j < data_blk_size;  j++ )
       { i = ( (int) ( crc_accum >> 24) ^ *data_blk_ptr++ ) & 0xff;
         crc_accum = ( crc_accum << 8 ) ^ crc_table[i]; }
   return crc_accum; }


// SM: block-level application
static int made_table = 0;
unsigned long crc32(unsigned char const *data, int length)
{
  if (!made_table) {
    gen_crc_table();
    made_table = 1;
  }

  return update_crc(0xFFFFFFFF, (char*)data, length);
}


// ----------------- test code ------------------------------
#ifdef TEST_CRC

#include <stdio.h>     // printf, FILE, etc.
#include <stdlib.h>    // malloc


int errors=0;

void testCrc(char const *data, int length, unsigned long crc)
{
  unsigned long val = crc32((unsigned char*)data, length);
  printf("computed crc is 0x%08lX, expected is 0x%08lX\n",
         val, ~crc);       // why is 'crc' inverted?
  if (val != ~crc) {
    errors++;
  }
}


int main(int argc, char *argv[])
{
  // if there's an argument, crc that
  if (argc >= 2) {
    FILE *fp = fopen(argv[1], "r");
    if (!fp) {
      printf("error opening %s: %m\n", argv[1]);
      return 2;
    }

    // get length
    fseek(fp, 0, SEEK_END);
    int len = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // read the entire contents
    unsigned char *buf = (unsigned char*)malloc(len);
    if (fread(buf, 1, len, fp) != (size_t)len) {
      printf("read error, or short count..\n");
      return 2;
    }

    // crc it
    long val = crc32(buf, len);
    printf("crc32: 0x%08lX\n", val);

    return 0;
  }

  /* 40 Octets filled with "0" */
  /* CPCS-UU = 0, CPI = 0, Length = 40, CRC-32 = 864d7f99 */
  char pkt_data1[48]={0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
                      0x00,0x00,0x00,0x28,0x86,0x4d,0x7f,0x99};

  /* 40 Octets filled with "1" */
  /* CPCS-UU = 0, CPI = 0, Length = 40, CRC-32 = c55e457a */
  char pkt_data2[48]={0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
                      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
                      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
                      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
                      0x00,0x00,0x00,0x28,0xc5,0x5e,0x45,0x7a};

  /* 40 Octets counting: 1 to 40 */
  /* CPCS-UU = 0, CPI = 0, Length = 40, CRC-32 = bf671ed0 */
  char pkt_data3[48]={0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,
                      0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,
                      0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,
                      0x1f,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,
                      0x00,0x00,0x00,0x28,0xbf,0x67,0x1e,0xd0};

  /* 40 Octets counting: 1 to 40 */
  /* CPCS-UU = 11, CPI = 22, CRC-32 = acba602a */
  char pkt_data4[48]={0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,
                      0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,
                      0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,
                      0x1f,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,
                      0x11,0x22,0x00,0x28,0xac,0xba,0x60,0x2a};

  testCrc(pkt_data1, 44, 0x864d7f99);
  testCrc(pkt_data2, 44, 0xc55e457a);
  testCrc(pkt_data3, 44, 0xbf671ed0);
  testCrc(pkt_data4, 44, 0xacba602a);

  return errors;
}

#endif // TEST_CRC



