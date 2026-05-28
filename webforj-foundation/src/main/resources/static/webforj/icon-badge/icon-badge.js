/**
 * webforJ Icon Badge — favicon overlay
 *
 * Paints a circle + count onto the document's existing favicon and swaps
 * the <link rel~="icon"> href to a data URL. The original href is cached
 * on first invocation and restored when the count is cleared.
 *
 * @since 26.01
 */
(function () {
  'use strict';

  if (window.__webforjIconBadgeReady) return;
  window.__webforjIconBadgeReady = true;

  const MIN_SIZE = 32;
  const BASE_RADIUS_FACTOR = 0.375;
  const SELECTOR = 'link[rel~="icon" i]';
  const ORIGINAL_ATTR = 'data-webforj-icon-original';

  const findLinks = () => document.querySelectorAll(SELECTOR);

  const stampOriginals = (links) => {
    for (const link of links) {
      if (!link.getAttribute(ORIGINAL_ATTR) && link.href) {
        link.setAttribute(ORIGINAL_ATTR, link.href);
      }
    }
  };

  const replaceAll = (links, hrefFor) => {
    for (const link of links) {
      try {
        const next = link.cloneNode();
        next.href = hrefFor(link);
        link.replaceWith(next);
      } catch (e) {
        /* canvas tainted, etc. — leave this link alone */
      }
    }
  };

  const autoContrast = (hex) => {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return (r * 299 + g * 587 + b * 114) / 1000 >= 128 ? '#000000' : '#ffffff';
  };

  const compose = (image, opts) => {
    const size = Math.max(MIN_SIZE, image.width || MIN_SIZE, image.height || MIN_SIZE);
    const canvas = document.createElement('canvas');
    canvas.width = size;
    canvas.height = size;
    const ctx = canvas.getContext('2d');
    ctx.drawImage(image, 0, 0, size, size);

    const r = Math.min(size * 0.5, size * BASE_RADIUS_FACTOR * (opts.size || 1));
    const cx = size - r;
    const cy = size - r;
    ctx.fillStyle = opts.color;
    if (opts.shape === 'square') {
      ctx.fillRect(cx - r, cy - r, r * 2, r * 2);
    } else {
      ctx.beginPath();
      ctx.arc(cx, cy, r, 0, Math.PI * 2);
      ctx.fill();
    }

    if (opts.label !== '') {
      const display = Number(opts.label) > 99 ? '99+' : String(opts.label);
      const fontPx = Math.max(7, Math.round(r * (display.length > 2 ? 1.1 : 1.4)));
      ctx.fillStyle = autoContrast(opts.color);
      ctx.font = `bold ${fontPx}px sans-serif`;
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText(display, cx, cy + 1);
    }

    return canvas.toDataURL('image/png');
  };

  const apply = (opts) => {
    const links = findLinks();
    if (links.length === 0) return;
    stampOriginals(links);

    const sourceHref = links[0].getAttribute(ORIGINAL_ATTR);
    if (!sourceHref) return;

    if (opts.count === undefined || opts.count === null) {
      replaceAll(links, (l) => l.getAttribute(ORIGINAL_ATTR));
      return;
    }

    const image = new Image();
    image.crossOrigin = 'anonymous';
    image.addEventListener('load', () => {
      const dataUrl = compose(image, {
        color: opts.color || '#e53935',
        shape: opts.shape || 'circle',
        size: opts.size || 1,
        label: opts.count
      });
      replaceAll(links, () => dataUrl);
    });
    image.addEventListener('error', () => { /* silent */ });
    image.src = sourceHref;
  };

  const pending = (window.__webforjIconBadge && window.__webforjIconBadge._q) || [];
  window.__webforjIconBadge = { apply };
  for (const opts of pending) apply(opts);
})();
