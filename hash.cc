/*
--------------------------------------------------------------------
lookup8.c, by Bob Jenkins, January 4 1997, Public Domain.
hash(), hash2(), hash3, and mix() are externally useful functions.
Routines to test the hash are included if SELF_TEST is defined.
You can use this free for any purpose.  It has no warranty.
--------------------------------------------------------------------
*/
// slight modifications by Noam Shazeer
// #define SELF_TEST

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
typedef  unsigned long  long uint64;   /* unsigned 8-byte quantities */
typedef  unsigned long  int  uint32;   /* unsigned 4-byte quantities */
typedef  unsigned       char uint8;

#define hashsize(n) ((uint64)1<<(n))
#define hashmask(n) (hashsize(n)-1)

/*
--------------------------------------------------------------------
mix -- mix 3 64-bit values reversibly.
mix() takes 48 machine instructions, but only 24 cycles on a superscalar
  machine (like Intel's new MMX architecture).  It requires 4 64-bit
  registers for 4::2 parallelism.
All 1-bit deltas, all 2-bit deltas, all deltas composed of top bits of
  (a,b,c), and all deltas of bottom bits were tested.  All deltas were
  tested both on random keys and on keys that were nearly all zero.
  These deltas all cause every bit of c to change between 1/3 and 2/3
  of the time (well, only 113/400 to 287/400 of the time for some
  2-bit delta).  These deltas all cause at least 80 bits to change
  among (a,b,c) when the mix is run either forward or backward (yes it
  is reversible).
This implies that a hash using mix64 has no funnels.  There may be
  characteristics with 3-bit deltas or bigger, I didn't test for
  those.
--------------------------------------------------------------------
*/
#define mix64(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>43); \
  b -= c; b -= a; b ^= (a<<9); \
  c -= a; c -= b; c ^= (b>>8); \
  a -= b; a -= c; a ^= (c>>38); \
  b -= c; b -= a; b ^= (a<<23); \
  c -= a; c -= b; c ^= (b>>5); \
  a -= b; a -= c; a ^= (c>>35); \
  b -= c; b -= a; b ^= (a<<49); \
  c -= a; c -= b; c ^= (b>>11); \
  a -= b; a -= c; a ^= (c>>12); \
  b -= c; b -= a; b ^= (a<<18); \
  c -= a; c -= b; c ^= (b>>22); \
}

/*
--------------------------------------------------------------------
hash() -- hash a variable-length key into a 64-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  level : can be any 8-byte value
Returns a 64-bit value.  Every bit of the key affects every bit of
the return value.  No funnels.  Every 1-bit and 2-bit delta achieves
avalanche.  About 41+5len instructions.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 64 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (uint8 **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hash( k[i], len[i], h);

By Bob Jenkins, Jan 4 1997.  bob_jenkins@burtleburtle.net.  You may
use this code any way you wish, private, educational, or commercial,
but I would appreciate if you give me credit.

See http://burtleburtle.net/bob/hash/evahash.html
Use for hash table lookup, or anything where one collision in 2^^64
is acceptable.  Do NOT use for cryptographic purposes.
--------------------------------------------------------------------
*/

uint64 BJHash( const uint8 *k, uint64 length, uint64 level) {
  register uint64 a,b,c,len;

  /* Set up the internal state */
  len = length;
  a = b = level;                         /* the previous hash value */
  c = 0x9e3779b97f4a7c13LL; /* the golden ratio; an arbitrary value */

  /*---------------------------------------- handle most of the key */
  while (len >= 24)
  { 
    a += (k[0]        +((uint64)k[ 1]<< 8)+((uint64)k[ 2]<<16)+((uint64)k[ 3]<<24)
     +((uint64)k[4 ]<<32)+((uint64)k[ 5]<<40)+((uint64)k[ 6]<<48)+((uint64)k[ 7]<<56));
    b += (k[8]        +((uint64)k[ 9]<< 8)+((uint64)k[10]<<16)+((uint64)k[11]<<24)
     +((uint64)k[12]<<32)+((uint64)k[13]<<40)+((uint64)k[14]<<48)+((uint64)k[15]<<56));
    c += (k[16]       +((uint64)k[17]<< 8)+((uint64)k[18]<<16)+((uint64)k[19]<<24)
     +((uint64)k[20]<<32)+((uint64)k[21]<<40)+((uint64)k[22]<<48)+((uint64)k[23]<<56));
    mix64(a,b,c);
    k += 24; len -= 24;
  }

  /*------------------------------------- handle the last 23 bytes */
  c += length;
  switch(len)              /* all the case statements fall through */
  {
  case 23: c+=((uint64)k[22]<<56);
  case 22: c+=((uint64)k[21]<<48);
  case 21: c+=((uint64)k[20]<<40);
  case 20: c+=((uint64)k[19]<<32);
  case 19: c+=((uint64)k[18]<<24);
  case 18: c+=((uint64)k[17]<<16);
  case 17: c+=((uint64)k[16]<<8);
    /* the first byte of c is reserved for the length */
  case 16: b+=((uint64)k[15]<<56);
  case 15: b+=((uint64)k[14]<<48);
  case 14: b+=((uint64)k[13]<<40);
  case 13: b+=((uint64)k[12]<<32);
  case 12: b+=((uint64)k[11]<<24);
  case 11: b+=((uint64)k[10]<<16);
  case 10: b+=((uint64)k[ 9]<<8);
  case  9: b+=((uint64)k[ 8]);
  case  8: a+=((uint64)k[ 7]<<56);
  case  7: a+=((uint64)k[ 6]<<48);
  case  6: a+=((uint64)k[ 5]<<40);
  case  5: a+=((uint64)k[ 4]<<32);
  case  4: a+=((uint64)k[ 3]<<24);
  case  3: a+=((uint64)k[ 2]<<16);
  case  2: a+=((uint64)k[ 1]<<8);
  case  1: a+=((uint64)k[ 0]);
    /* case 0: nothing left to add */
  }
  mix64(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}

/*
--------------------------------------------------------------------
 This works on all machines, is identical to hash() on little-endian 
 machines, and it is much faster than hash(), but it requires
 -- that the key be an array of uint64's, and
 -- that all your machines have the same endianness, and
 -- that the length be the number of uint64's in the key
--------------------------------------------------------------------
*/
uint64 hash2( uint64 *k, uint64 length, uint64 level) {
  register uint64 a,b,c,len;

  /* Set up the internal state */
  len = length;
  a = b = level;                         /* the previous hash value */
  c = 0x9e3779b97f4a7c13LL; /* the golden ratio; an arbitrary value */

  /*---------------------------------------- handle most of the key */
  while (len >= 3)
  {
    a += k[0];
    b += k[1];
    c += k[2];
    mix64(a,b,c);
    k += 3; len -= 3;
  }

  /*-------------------------------------- handle the last 2 uint64's */
  c += length;
  switch(len)              /* all the case statements fall through */
  {
    /* c is reserved for the length */
  case  2: b+=k[1];
  case  1: a+=k[0];
    /* case 0: nothing left to add */
  }
  mix64(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}

/*
--------------------------------------------------------------------
 This is identical to hash() on little-endian machines, and it is much
 faster than hash(), but a little slower than hash2(), and it requires
 -- that all your machines be little-endian, for example all Intel x86
    chips or all VAXen.  It gives wrong results on big-endian machines.
--------------------------------------------------------------------
*/

uint64 hash3( uint8 *k, uint64 length, uint64 level)
// register uint8 *k;        /* the key */
// register uint64  length;   /* the length of the key */
// register uint64  level;    /* the previous hash, or an arbitrary value */
{
  register uint64 a,b,c,len;

  /* Set up the internal state */
  len = length;
  a = b = level;                         /* the previous hash value */
  c = 0x9e3779b97f4a7c13LL; /* the golden ratio; an arbitrary value */

  /*---------------------------------------- handle most of the key */
  if (((size_t)k)&7)
  {
    while (len >= 24)
    {
      a += (k[0]        +((uint64)k[ 1]<< 8)+((uint64)k[ 2]<<16)+((uint64)k[ 3]<<24)
       +((uint64)k[4 ]<<32)+((uint64)k[ 5]<<40)+((uint64)k[ 6]<<48)+((uint64)k[ 7]<<56));
      b += (k[8]        +((uint64)k[ 9]<< 8)+((uint64)k[10]<<16)+((uint64)k[11]<<24)
       +((uint64)k[12]<<32)+((uint64)k[13]<<40)+((uint64)k[14]<<48)+((uint64)k[15]<<56));
      c += (k[16]       +((uint64)k[17]<< 8)+((uint64)k[18]<<16)+((uint64)k[19]<<24)
       +((uint64)k[20]<<32)+((uint64)k[21]<<40)+((uint64)k[22]<<48)+((uint64)k[23]<<56));
      mix64(a,b,c);
      k += 24; len -= 24;
    }
  }
  else
  {
    while (len >= 24)    /* aligned */
    {
      a += *(uint64 *)(k+0);
      b += *(uint64 *)(k+8);
      c += *(uint64 *)(k+16);
      mix64(a,b,c);
      k += 24; len -= 24;
    }
  }

  /*------------------------------------- handle the last 23 bytes */
  c += length;
  switch(len)              /* all the case statements fall through */
  {
  case 23: c+=((uint64)k[22]<<56);
  case 22: c+=((uint64)k[21]<<48);
  case 21: c+=((uint64)k[20]<<40);
  case 20: c+=((uint64)k[19]<<32);
  case 19: c+=((uint64)k[18]<<24);
  case 18: c+=((uint64)k[17]<<16);
  case 17: c+=((uint64)k[16]<<8);
    /* the first byte of c is reserved for the length */
  case 16: b+=((uint64)k[15]<<56);
  case 15: b+=((uint64)k[14]<<48);
  case 14: b+=((uint64)k[13]<<40);
  case 13: b+=((uint64)k[12]<<32);
  case 12: b+=((uint64)k[11]<<24);
  case 11: b+=((uint64)k[10]<<16);
  case 10: b+=((uint64)k[ 9]<<8);
  case  9: b+=((uint64)k[ 8]);
  case  8: a+=((uint64)k[ 7]<<56);
  case  7: a+=((uint64)k[ 6]<<48);
  case  6: a+=((uint64)k[ 5]<<40);
  case  5: a+=((uint64)k[ 4]<<32);
  case  4: a+=((uint64)k[ 3]<<24);
  case  3: a+=((uint64)k[ 2]<<16);
  case  2: a+=((uint64)k[ 1]<<8);
  case  1: a+=((uint64)k[ 0]);
    /* case 0: nothing left to add */
  }
  mix64(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}

#ifdef SELF_TEST

/* used for timings */
void driver1()
{
  uint64 buf[256];
  uint64 i;
  uint64 h=0;

  for (i=0; i<256; ++i) 
  {
    h = BJHash((const uint8*)buf,i,h);
  }
}

/* check that every input bit changes every output bit half the time */
#define HASHSTATE 1
#define HASHLEN   1
#define MAXPAIR 80
#define MAXLEN 5
void driver2()
{
  uint8 qa[MAXLEN+1], qb[MAXLEN+2], *a = &qa[0], *b = &qb[1];
  uint64 c[HASHSTATE], d[HASHSTATE], i, j=0, k, l, m=0, z;
  uint64 e[HASHSTATE],f[HASHSTATE],g[HASHSTATE],h[HASHSTATE];
  uint64 x[HASHSTATE],y[HASHSTATE];
  uint64 hlen;

  printf("No more than %d trials should ever be needed \n",MAXPAIR/2);
  for (hlen=0; hlen < MAXLEN; ++hlen)
  {
    z=0;
    for (i=0; i<hlen; ++i)  /*----------------------- for each byte, */
    {
      for (j=0; j<8; ++j)   /*------------------------ for each bit, */
      {
	for (m=0; m<8; ++m) /*-------- for serveral possible levels, */
	{
	  for (l=0; l<HASHSTATE; ++l) e[l]=f[l]=g[l]=h[l]=x[l]=y[l]=~((uint64)0);

      	  /*---- check that every input bit affects every output bit */
	  for (k=0; k<MAXPAIR; k+=2)
	  { 
	    uint64 finished=1;
	    /* keys have one bit different */
	    for (l=0; l<hlen+1; ++l) {a[l] = b[l] = (uint8)0;}
	    /* have a and b be two keys differing in only one bit */
	    a[i] ^= (k<<j);
	    a[i] ^= (k>>(8-j));
	     c[0] = BJHash(a, hlen, m);
	    b[i] ^= ((k+1)<<j);
	    b[i] ^= ((k+1)>>(8-j));
	     d[0] = BJHash(b, hlen, m);
	    /* check every bit is 1, 0, set, and not set at least once */
	    for (l=0; l<HASHSTATE; ++l)
	    {
	      e[l] &= (c[l]^d[l]);
	      f[l] &= ~(c[l]^d[l]);
	      g[l] &= c[l];
	      h[l] &= ~c[l];
	      x[l] &= d[l];
	      y[l] &= ~d[l];
	      if (e[l]|f[l]|g[l]|h[l]|x[l]|y[l]) finished=0;
	    }
	    if (finished) break;
	  }
	  if (k>z) z=k;
	  if (k==MAXPAIR) 
	  {
	     printf("Some bit didn't change: ");
	     printf("%lld %lld %lld %lld %lld %lld  ",
	            e[0],f[0],g[0],h[0],x[0],y[0]);
	     printf("i %ld j %ld m %ld len %ld\n",
	            (uint32)i,(uint32)j,(uint32)m,(uint32)hlen);
	  }
	  if (z==MAXPAIR) goto done;
	}
      }
    }
   done:
    if (z < MAXPAIR)
    {
      printf("Mix success  %2ld bytes  %2ld levels  ",(uint32)i,(uint32)m);
      printf("required  %ld  trials\n",(uint32)(z/2));
    }
  }
  printf("\n");
}

/* Check for reading beyond the end of the buffer and alignment problems */
void driver3()
{
  uint8 buf[MAXLEN+20], *b;
  uint64 len;
  uint8 q[] = "This is the time for all good men to come to the aid of their country";
  uint8 qq[] = "xThis is the time for all good men to come to the aid of their country";
  uint8 qqq[] = "xxThis is the time for all good men to come to the aid of their country";
  uint8 qqqq[] = "xxxThis is the time for all good men to come to the aid of their country";
  uint8 o[] = "xxxxThis is the time for all good men to come to the aid of their country";
  uint8 oo[] = "xxxxxThis is the time for all good men to come to the aid of their country";
  uint8 ooo[] = "xxxxxxThis is the time for all good men to come to the aid of their country";
  uint8 oooo[] = "xxxxxxxThis is the time for all good men to come to the aid of their country";
  uint64 h,i,j,ref,x,y;

  printf("Endianness.  These should all be the same:\n");
  h = BJHash(q+0, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(qq+1, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(qqq+2, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(qqqq+3, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(o+4, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(oo+5, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(ooo+6, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  h = BJHash(oooo+7, (uint64)(sizeof(q)-1), (uint64)0);
  printf("%.8lx%.8lx\n", (uint32)h, (uint32)(h>>32));
  printf("\n");
  for (h=0, b=buf+1; h<8; ++h, ++b)
  {
    for (i=0; i<MAXLEN; ++i)
    {
      len = i;
      for (j=0; j<i; ++j) *(b+j)=0;

      /* these should all be equal */
      ref = BJHash(b, len, (uint64)1);
      *(b+i)=(uint8)~0;
      *(b-1)=(uint8)~0;
      x = BJHash(b, len, (uint64)1);
      y = BJHash(b, len, (uint64)1);
      if ((ref != x) || (ref != y)) 
      {
	printf("alignment error: %lld %lld %lld %lld %lld\n",ref,x,y,h,i);
      }
    }
  }
}

/* check for problems with nulls */
 void driver4()
{
  uint8 buf[1];
  uint64 h,i,state[HASHSTATE];


  buf[0] = ~0;
  for (i=0; i<HASHSTATE; ++i) state[i] = 1;
  printf("These should all be different\n");
  for (i=0, h=0; i<8; ++i)
  {
    h = BJHash(buf, (uint64)0, h);
    printf("%2ld  0-byte strings, hash is  %.8lx%.8lx\n", (uint32)i,
      (uint32)h,(uint32)(h>>32));
  }
}


int main()
{
  driver1();   /* test that the key is hashed: used for timings */
  driver2();   /* test that whole key is hashed thoroughly */
  driver3();   /* test that nothing but the key is hashed */
  driver4();   /* test hashing multiple buffers (all buffers are null) */
  return 1;
}

#endif  /* SELF_TEST */
