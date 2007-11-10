/* gprintf.c */
/* originally from: http://www.efgh.com/software/gprintf.htm */
/* this file is in the public domain */

/* modified by Scott McPeak, April 2003:
 *   - use va_list instead of 'const int*' for the
 *     pointer-to-argument type (for portability)
 *   - implement conservative estimates for unknown format
 *     chars, particularly 'f' (CONSERVATIVE_ESTIMATE flag)
 *   - add a few test vectors
 */

/* NOTE: There are quite a few differences among the *printf
 * implementations running around in the various libcs.  The
 * implementation in this module doesn't know about all of those
 * variations and extensions.  So, if you're using this to estimate
 * the # of chars your libc's printf will use, be sure to compare
 * libc's printf's actual return value, to make sure something doesn't
 * slip through the cracks. */

/* Code for general_printf() */
/* Change extension to .c before compiling */

#include "sm_gprintf.h"
#include <assert.h>      /* assert */

/* when this is true, unknown fields are filled with Xs, in an attempt
 * to print at least as many characters as libc's sprintf */
#define CONSERVATIVE_ESTIMATE 1

#define BITS_PER_BYTE           8

struct parameters
{
  int number_of_output_chars;
  short minimum_field_width;
  char options;
    #define MINUS_SIGN    1
    #define RIGHT_JUSTIFY 2
    #define ZERO_PAD      4
    #define CAPITAL_HEX   8
  short edited_sm_string_length;
  short leading_zeros;
  int (*output_function)(void *, int);
  void *output_pointer;
};

static void output_and_count(struct parameters *p, int c)
{
  if (p->number_of_output_chars >= 0)
  {
    int n = (*p->output_function)(p->output_pointer, c);
    if (n>=0) p->number_of_output_chars++;
    else p->number_of_output_chars = n;
  }
}

static void output_field(struct parameters *p, char const *s)
{
  short justification_length =
    p->minimum_field_width - p->leading_zeros - p->edited_sm_string_length;
  if (p->options & MINUS_SIGN)
  {
    if (p->options & ZERO_PAD)
      output_and_count(p, '-');
    justification_length--;
  }
  if (p->options & RIGHT_JUSTIFY)
    while (--justification_length >= 0)
      output_and_count(p, p->options & ZERO_PAD ? '0' : ' ');
  if (p->options & MINUS_SIGN && !(p->options & ZERO_PAD))
    output_and_count(p, '-');
  while (--p->leading_zeros >= 0)
    output_and_count(p, '0');
  while (--p->edited_sm_string_length >= 0)
    output_and_count(p, *s++);
  while (--justification_length >= 0)
    output_and_count(p, ' ');
}


int general_vprintf(Gprintf_output_function output_function,
                    void *output_pointer,
                    const char *control_sm_string,
                    va_list argument_pointer)
{
  struct parameters p;
  char control_char;
  p.number_of_output_chars = 0;
  p.output_function = output_function;
  p.output_pointer = output_pointer;
  control_char = *control_sm_string++;
  while (control_char != '\0')
  {
    if (control_char == '%')
    {
      short precision = -1;
      short long_argument = 0;
      short base = 0;
      control_char = *control_sm_string++;
      p.minimum_field_width = 0;
      p.leading_zeros = 0;
      p.options = RIGHT_JUSTIFY;
      if (control_char == '-')
      {
        p.options = 0;
        control_char = *control_sm_string++;
      }
      if (control_char == '0')
      {
        p.options |= ZERO_PAD;
        control_char = *control_sm_string++;
      }
      if (control_char == '*')
      {
        p.minimum_field_width = va_arg(argument_pointer, int);
        control_char = *control_sm_string++;
      }
      else
      {
        while ('0' <= control_char && control_char <= '9')
        {
          p.minimum_field_width =
            p.minimum_field_width * 10 + control_char - '0';
          control_char = *control_sm_string++;
        }
      }
      if (control_char == '.')
      {
        control_char = *control_sm_string++;
        if (control_char == '*')
        {
          precision = va_arg(argument_pointer, int);
          control_char = *control_sm_string++;
        }
        else
        {
          precision = 0;
          while ('0' <= control_char && control_char <= '9')
          {
            precision = precision * 10 + control_char - '0';
            control_char = *control_sm_string++;
          }
        }
      }
      if (control_char == 'l')
      {
        long_argument = 1;
        control_char = *control_sm_string++;
      }
      if (control_char == 'd')
        base = 10;
      else if (control_char == 'x')
        base = 16;
      else if (control_char == 'X')
      {
        base = 16;
        p.options |= CAPITAL_HEX;
      }
      else if (control_char == 'u')
        base = 10;
      else if (control_char == 'o')
        base = 8;
      else if (control_char == 'b')
        base = 2;
      else if (control_char == 'c')
      {
        base = -1;
        p.options &= ~ZERO_PAD;
      }
      else if (control_char == 's')
      {
        base = -2;
        p.options &= ~ZERO_PAD;
      }
      if (base == 0)  /* invalid conversion type */
      {
        if (control_char != '\0')
        {
          #if !CONSERVATIVE_ESTIMATE
            /* sm: this was the original code; it just prints the
             * format character itself */
            output_and_count(&p, control_char);

          #else
            /* since my goal is actually to compute a conservative
             * upper bound on the # of chars output by sprintf, I want
             * to fill unknown fields with Xs */
            static char const * const XXX =
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";  /* 50 Xs */
            assert(precision <= 30);     /* otherwise I need more Xs */

            /* I'm assuming that printing floating-point is the worst case.
             * I further assume non-fractional parts (integer part,
             * exponent, decimal, sign) won't exceed 20 chars.  Finally,
             * up to 30 characters of decimal part are supported (this
             * is checked with the assertion above). */
            if (precision == -1) {
              p.edited_sm_string_length = 20 + 6;    /* 6 is default precision for 'f' */
            }
            else {
              p.edited_sm_string_length = 20 + precision;
            }
            output_field(&p, XXX);
          #endif

          control_char = *control_sm_string++;
        }
      }
      else
      {
        if (base == -1)  /* conversion type c */
        {
          /* 'char' is passed as 'int' through '...' */
          char c = (char)va_arg(argument_pointer, int);
          p.edited_sm_string_length = 1;
          output_field(&p, &c);
        }
        else if (base == -2)  /* conversion type s */
        {
          char *sm_string;
          p.edited_sm_string_length = 0;
          sm_string = va_arg(argument_pointer, char*);
          while (sm_string[p.edited_sm_string_length] != 0)
            p.edited_sm_string_length++;
          if (precision >= 0 && p.edited_sm_string_length > precision)
            p.edited_sm_string_length = precision;
          output_field(&p, sm_string);
        }
        else  /* conversion type d, b, o or x */
        {
          unsigned long x;
          char buffer[BITS_PER_BYTE * sizeof(unsigned long) + 1];
          p.edited_sm_string_length = 0;
          if (long_argument)
          {
            x = va_arg(argument_pointer, unsigned long);
          }
          else if (control_char == 'd')
            x = va_arg(argument_pointer, long);
          else
            x = va_arg(argument_pointer, unsigned);
          if (control_char == 'd' && (long) x < 0)
          {
            p.options |= MINUS_SIGN;
            x = - (long) x;
          }
          do
          {
            int c;
            c = x % base + '0';
            if (c > '9')
            {
              if (p.options & CAPITAL_HEX)
                c += 'A'-'9'-1;
              else
                c += 'a'-'9'-1;
            }
            buffer[sizeof(buffer) - 1 - p.edited_sm_string_length++] = c;
          }
          while ((x/=base) != 0);
          if (precision >= 0 && precision > p.edited_sm_string_length)
            p.leading_zeros = precision - p.edited_sm_string_length;
          output_field(&p, buffer + sizeof(buffer) - p.edited_sm_string_length);
        }
        control_char = *control_sm_string++;
      }
    }
    else
    {
      output_and_count(&p, control_char);
      control_char = *control_sm_string++;
    }
  }
  return p.number_of_output_chars;
}


int general_printf(Gprintf_output_function output,
                   void *extra, const char *format, ...)
{
  va_list args;
  int ret;

  va_start(args, format);
  ret = general_vprintf(output, extra, format, args);
  va_end(args);

  return ret;
}


/* ------------------ test code --------------------- */
#ifdef TEST_GPRINTF

#include <stdio.h>     /* fputc, printf, vsprintf */
#include <cstring>    /* strcmp, std::strlen */
#include <stdlib.h>    /* exit */


int sm_string_output(void *extra, int ch)
{
  /* the 'extra' argument is a pointer to a pointer to the
   * next character to write */
  char **s = (char**)extra;

  **s = ch;     /* write */
  (*s)++;       /* advance */

  return 0;
}

int general_vsprintf(char *dest, char const *format, va_list args)
{
  char *s = dest;
  int ret;

  ret = general_vprintf(sm_string_output, &s, format, args);
  *s = 0;

  return ret;
}


char output1[1024];    /* for libc */
char output2[1024];    /* for this module */


void expect_vector_len(int expect_len, char const *expect_output,
                       char const *format, va_list args)
{
  int len;
  static int vectors = 0;

  /* keep track of how many vectors we've tried, to make it
   * a little easier to correlate failures with the inputs
   * in this file */
  vectors++;

  /* run the generalized vsprintf */
  len = general_vsprintf(output2, format, args);

  /* compare */
  if (len!=expect_len ||
      0!=strcmp(expect_output, output2)) {
    printf("outputs differ for vector %d!\n", vectors);
    printf("  format: %s\n", format);
    printf("  expect: %s (%d)\n", expect_output, expect_len);
    printf("      me: %s (%d)\n", output2, len);
    exit(2);
  }
}


void expect_vector(char const *expect_output,
                   char const *format, ...)
{
  va_list args;
  va_start(args, format);
  expect_vector_len(std::strlen(expect_output), expect_output, format, args);
  va_end(args);
}


void vector(char const *format, ...)
{
  va_list args;
  int len;

  /* run the real vsprintf */
  va_start(args, format);
  len = vsprintf(output1, format, args);
  va_end(args);

  /* test against the generalized vsprintf */
  va_start(args, format);
  expect_vector_len(len, output1, format, args);
  va_end(args);
}


int main()
{
  printf("testing gprintf...\n");

  /* test against libc */
  vector("simple");
  vector("a %s more", "little");
  vector("some %4d more %s complicated %c stuff",
         33, "yikes", 'f');

  /* test unknown format chars */
  expect_vector("XXXXXXXXXXXXXXXXXXXXXXXXXX", "%f", 3.4);
  expect_vector("XXXXXXXXXXXXXXXXXXXXXXX", "%.3f", 3.4);
  expect_vector("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "%.10f", 3.4);
  expect_vector("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "%.30f", 3.4);

  /* fails assertion, as it should */
  /* expect_vector("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", "%.31f", 3.4); */

  /* TODO: add more tests */

  printf("gprintf works\n");
  return 0;
}

#endif /* TEST_GPRINTF */
