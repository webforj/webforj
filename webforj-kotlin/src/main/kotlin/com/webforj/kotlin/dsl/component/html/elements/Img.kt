package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Img
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Img` with an optional [src] and / or [alt] value.
 * ```
 * ... {
 *   img() // Empty img element
 *   img("src") // img element with source
 *   img(alt = "alt") // img element with alt text
 *   img("src", "alt") // img with source and alt text
 * }
 * ```
 * @param src The source of the image.
 * @param alt The alt text of the image.
 * @param block The initialization steps for the `Img`.
 * @return The configured `Img` instance.
 * @see Img
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img">HTML img Tag</a>
 */
fun @WebforjDsl HasComponents.img(src: String? = null, alt: String? = null, block: @WebforjDsl Img.() -> Unit = {}): Img {
    val img = if (alt != null && src != null) {
        Img(src, alt)
    } else if (src != null) {
        Img(src)
    } else {
        Img().apply { alt?.let { setAlt(it) } }
    }
    return init(img, block)
}
