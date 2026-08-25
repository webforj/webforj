package com.webforj.kotlin.dsl.component.card

import com.webforj.component.Expanse
import com.webforj.component.button.Button
import com.webforj.component.button.ButtonTheme
import com.webforj.component.card.Card
import com.webforj.component.element.event.ElementClickEvent
import com.webforj.component.html.elements.Div
import com.webforj.component.html.elements.H3
import com.webforj.component.html.elements.Img
import com.webforj.component.html.elements.Paragraph
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.avatar.avatar
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.html.elements.h3
import com.webforj.kotlin.dsl.component.html.elements.img
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class CardTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateCardWithDefaults() {
    val card = root.card()

    assertNotNull(card)
    assertTrue { root.hasComponent(card) }
    assertEquals(Card.Shadow.XSMALL, card.shadow)
    assertEquals(Expanse.MEDIUM, card.expanse)
    assertEquals(Card.Orientation.VERTICAL, card.orientation)
    assertFalse(card.isDivided)
    assertFalse(card.isBorderless)
    assertEquals(0, card.componentCount)
  }

  @Test
  fun shouldCreateCardWithShadow() {
    val card = root.card(shadow = Card.Shadow.XXLARGE)

    assertEquals(Card.Shadow.XXLARGE, card.shadow)
  }

  @Test
  fun shouldCreateCardWithExpanse() {
    val card = root.card(expanse = Expanse.SMALL)

    assertEquals(Expanse.SMALL, card.expanse)
  }

  @Test
  fun shouldCreateCardWithOrientation() {
    val card = root.card(orientation = Card.Orientation.HORIZONTAL)

    assertEquals(Card.Orientation.HORIZONTAL, card.orientation)
  }

  @Test
  fun shouldCreateCardWithDivided() {
    val card = root.card(divided = true)

    assertTrue(card.isDivided)
  }

  @Test
  fun shouldCreateCardWithBorderless() {
    val card = root.card(borderless = true)

    assertTrue(card.isBorderless)
  }

  @Test
  fun shouldCreateCardWithAllSettings() {
    val card =
      root.card(
        shadow = Card.Shadow.NONE,
        expanse = Expanse.LARGE,
        orientation = Card.Orientation.HORIZONTAL,
        divided = true,
        borderless = true,
      )

    assertEquals(Card.Shadow.NONE, card.shadow)
    assertEquals(Expanse.LARGE, card.expanse)
    assertEquals(Card.Orientation.HORIZONTAL, card.orientation)
    assertTrue(card.isDivided)
    assertTrue(card.isBorderless)
  }

  @Test
  fun shouldConfigureCardInsideBlock() {
    val card = root.card {
      setShadow(Card.Shadow.MEDIUM)
      setOrientation(Card.Orientation.HORIZONTAL)
    }

    assertEquals(Card.Shadow.MEDIUM, card.shadow)
    assertEquals(Card.Orientation.HORIZONTAL, card.orientation)
  }

  @Test
  fun shouldCreateFigure() {
    lateinit var cover: Img
    val card = root.card {
      figureSlot {
        cover = img("cover.png", "A cover")
      }
    }

    assertTrue { root.hasComponent(card) }
    assertTrue { card.hasComponent(cover) }
    assertEquals("cover.png", cover.src)
  }

  @Test
  fun shouldCreateIcon() {
    val card = root.card {
      iconSlot {
        avatar("Aang", "AA")
      }
    }

    assertEquals(1, card.componentCount)
  }

  @Test
  fun shouldCreateTitle() {
    lateinit var title: H3
    val card = root.card {
      titleSlot {
        title = h3("Card Title")
      }
    }

    assertTrue { card.hasComponent(title) }
    assertEquals("Card Title", title.text)
  }

  @Test
  fun shouldCreateCaption() {
    lateinit var caption: Paragraph
    val card = root.card {
      captionSlot {
        caption = paragraph("Card caption")
      }
    }

    assertTrue { card.hasComponent(caption) }
    assertEquals("Card caption", caption.text)
  }

  @Test
  fun shouldCreateHeaderActions() {
    lateinit var share: Button
    lateinit var save: Button
    val card = root.card {
      headerActionsSlot {
        share = button("Share")
        save = button("Save", theme = ButtonTheme.PRIMARY)
      }
    }

    assertTrue { card.hasComponent(share) }
    assertTrue { card.hasComponent(save) }
    assertEquals(2, card.componentCount)
  }

  @Test
  fun shouldCreateFooter() {
    lateinit var readMore: Button
    val card = root.card {
      footerSlot {
        readMore = button("Read more")
      }
    }

    assertTrue { card.hasComponent(readMore) }
    assertEquals("Read more", readMore.text)
  }

  @Test
  fun shouldCreateBody() {
    lateinit var body: Paragraph
    val card = root.card {
      body = paragraph("Card body content")
    }

    assertTrue { card.hasComponent(body) }
    assertEquals(1, card.componentCount)
  }

  @Test
  fun shouldRemoveSlottedComponent() {
    lateinit var readMore: Button
    val card = root.card {
      footerSlot {
        readMore = button("Read more")
      }
    }

    card.remove(readMore)

    assertFalse(card.hasComponent(readMore))
  }

  @Test
  fun shouldAddClickListener() {
    val card = root.card {
      onClick {}
    }

    assertEquals(1, card.getEventListeners(ElementClickEvent::class.java).size)
  }

  @Test
  fun shouldRemoveClickListener() {
    val card = root.card()
    val registration = card.addClickListener {}

    assertEquals(1, card.getEventListeners(ElementClickEvent::class.java).size)

    registration.remove()

    assertEquals(0, card.getEventListeners(ElementClickEvent::class.java).size)
  }

  @Test
  fun shouldCreateExample() {
    val card =
      root.card(
        shadow = Card.Shadow.LARGE,
        expanse = Expanse.MEDIUM,
        orientation = Card.Orientation.HORIZONTAL,
        divided = true,
      ) {
        figureSlot {
          img("cover.png", "A cover")
        }
        iconSlot {
          avatar("Aang", "AA")
        }
        titleSlot {
          h3("Card Title")
        }
        captionSlot {
          paragraph("Card caption")
        }
        headerActionsSlot {
          button("Share")
        }
        paragraph("Card body content")
        footerSlot {
          button("Read more")
        }
        onClick {}
      }

    assertTrue { root.hasComponent(card) }
    assertEquals(7, card.componentCount)
    assertEquals(Card.Shadow.LARGE, card.shadow)
    assertEquals(Card.Orientation.HORIZONTAL, card.orientation)
    assertTrue(card.isDivided)
    assertEquals(1, card.getEventListeners(ElementClickEvent::class.java).size)
  }
}
