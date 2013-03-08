
/*
 * Copyright (c) 2011, Vicent Marti
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include "flx_sundown.hpp"
#include "markdown.h"
#include "html.h"
#include "buffer.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#define READ_UNIT 1024
#define OUTPUT_UNIT 64

::std::string sundown (::std::string const &inp)
{
	struct buf *ob;

	struct sd_callbacks callbacks;
	struct html_renderopt options;
	struct sd_markdown *markdown;

	/* performing markdown parsing */
	ob = bufnew(OUTPUT_UNIT);

	sdhtml_renderer(&callbacks, &options, 0);
	markdown = sd_markdown_new(0, 16, &callbacks, &options);

	sd_markdown_render(ob, (uint8_t*)inp.data(), inp.size(), markdown);
	sd_markdown_free(markdown);

	/* writing the result to stdout */
	::std::string ret = ::std::string ((char*)ob->data,ob->size);

	bufrelease(ob);
  return ret;
}

/* vim: set filetype=c: */
