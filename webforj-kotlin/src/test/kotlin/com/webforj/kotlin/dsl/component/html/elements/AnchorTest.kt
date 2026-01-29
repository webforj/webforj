package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class AnchorTest {
    lateinit var root: Div

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @Test
    fun shouldCreateAnchorWithHref() {
        val expected = "href"
        val anchor = root.anchor(expected)
        assertEquals(expected, anchor.href)
        assertEquals("", anchor.text)
        assertEquals("_self", anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithText() {
        val expected = "text"
        val anchor = root.anchor(text = expected)
        assertEquals("", anchor.href)
        assertEquals(expected, anchor.text)
        assertEquals("_self", anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithTarget() {
        val expected = "target"
        val anchor = root.anchor(target = expected)
        assertEquals("", anchor.href)
        assertEquals("", anchor.text)
        assertEquals(expected, anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithHrefAndText() {
        val expectedHref = "href"
        val expectedText = "text"
        val anchor = root.anchor(expectedHref, expectedText)
        assertEquals(expectedHref, anchor.href)
        assertEquals(expectedText, anchor.text)
        assertEquals("_self", anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithHrefAndTarget() {
        val expectedHref = "href"
        val expectedTarget = "target"
        val anchor = root.anchor(expectedHref, target = expectedTarget)
        assertEquals(expectedHref, anchor.href)
        assertEquals("", anchor.text)
        assertEquals(expectedTarget, anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithTextAndTarget() {
        val expectedText = "text"
        val expectedTarget = "target"
        val anchor = root.anchor(text = expectedText, target = expectedTarget)
        assertEquals("", anchor.href)
        assertEquals(expectedText, anchor.text)
        assertEquals(expectedTarget, anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

    @Test
    fun shouldCreateAnchorWithHrefAndTextAndTarget() {
        val expectedHref = "href"
        val expectedText = "text"
        val expectedTarget = "target"
        val anchor = root.anchor(expectedHref, expectedText, expectedTarget)
        assertEquals(expectedHref, anchor.href)
        assertEquals(expectedText, anchor.text)
        assertEquals(expectedTarget, anchor.target)
        assertTrue { root.hasComponent(anchor) }
    }

}
