package com.webforj.kotlin.dsl.component.card

import com.webforj.component.Expanse
import com.webforj.component.card.Card
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Card` component with optional [shadow], [expanse], [orientation], [divided], and/or
 * [borderless] settings.
 *
 * The components added directly inside the block make up the card's body. Every other region is
 * filled through its own slot and a region without content is not rendered at all.
 *
 * ```
 * ... {
 *   card() // Empty Card component
 *   card(shadow = Card.Shadow.LARGE) // Card with elevation
 *   card(expanse = Expanse.SMALL, orientation = Card.Orientation.HORIZONTAL) // Compact horizontal Card
 *   card(divided = true, borderless = true) // Card with dividers and no frame ring
 *   card {
 *     figureSlot {
 *       img("cover.png")
 *     }
 *     iconSlot {
 *       featherIcon(FeatherIcon.BOOK)
 *     }
 *     titleSlot {
 *       h3("Card Title")
 *     }
 *     captionSlot {
 *       paragraph("Card caption")
 *     }
 *     headerActionsSlot {
 *       button("Share")
 *     }
 *     paragraph("Card body content")
 *     footerSlot {
 *       button("Read more")
 *     }
 *     onClick { println("Card clicked") }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Card` see
 * - [figureSlot],
 * - [iconSlot],
 * - [titleSlot],
 * - [captionSlot],
 * - [headerActionsSlot], and
 * - [footerSlot]
 *
 * @param shadow The elevation of the `Card` (e.g., Card.Shadow.NONE, Card.Shadow.LARGE).
 * @param expanse The expanse of the `Card` driving its padding and gaps (e.g., Expanse.SMALL).
 * @param orientation The orientation of the `Card` (e.g., Card.Orientation.HORIZONTAL).
 * @param divided Whether the `Card` draws dividers between its regions.
 * @param borderless Whether the `Card` is drawn without its frame ring.
 * @param block The initialization steps of the `Card`.
 * @return The configured `Card`.
 * @see Card
 */
fun @WebforjDsl HasComponents.card(
  shadow: Card.Shadow? = null,
  expanse: Expanse? = null,
  orientation: Card.Orientation? = null,
  divided: Boolean? = null,
  borderless: Boolean? = null,
  block: @WebforjDsl Card.() -> Unit = {},
): Card {
  val card =
    Card().apply {
      shadow?.let { setShadow(it) }
      expanse?.let { setExpanse(it) }
      orientation?.let { setOrientation(it) }
      divided?.let { setDivided(it) }
      borderless?.let { setBorderless(it) }
    }
  return init(card, block)
}

/**
 * Configures the components to add to the figure slot of a `Card`.
 *
 * The figure holds the card's illustration, for instance an image, a video or a chart.
 *
 * ```
 * card {
 *   figureSlot {
 *     img("cover.png")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the figure components.
 */
fun @WebforjDsl Card.figureSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToFigure)
}

/**
 * Configures the components to add to the icon slot of a `Card`.
 *
 * The icon is the leading visual of the header row.
 *
 * ```
 * card {
 *   iconSlot {
 *     avatar("Aang", "AA")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the icon components.
 */
fun @WebforjDsl Card.iconSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToIcon)
}

/**
 * Configures the components to add to the title slot of a `Card`.
 *
 * The title names the card and is also used as the card's accessible name.
 *
 * ```
 * card {
 *   titleSlot {
 *     h3("Card Title")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the title components.
 */
fun @WebforjDsl Card.titleSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToTitle)
}

/**
 * Configures the components to add to the caption slot of a `Card`.
 *
 * The caption is a short secondary line rendered under the title.
 *
 * ```
 * card {
 *   captionSlot {
 *     paragraph("Card caption")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the caption components.
 */
fun @WebforjDsl Card.captionSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToCaption)
}

/**
 * Configures the components to add to the header actions slot of a `Card`.
 *
 * The header actions sit at the end of the header row and typically hold buttons or a menu.
 *
 * ```
 * card {
 *   headerActionsSlot {
 *     button("Share")
 *     button("Save", theme = ButtonTheme.PRIMARY)
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the header actions components.
 */
fun @WebforjDsl Card.headerActionsSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToHeaderActions)
}

/**
 * Configures the components to add to the footer slot of a `Card`.
 *
 * The footer closes the card and typically holds actions or metadata.
 *
 * ```
 * card {
 *   footerSlot {
 *     button("Read more")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footer components.
 */
fun @WebforjDsl Card.footerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Card::addToFooter)
}
