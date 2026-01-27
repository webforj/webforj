package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Div
import com.webforj.component.html.elements.Fieldset
import com.webforj.component.html.elements.Legend
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class FieldsetTest {
    lateinit var root: Div
    lateinit var fieldset: Fieldset

    @BeforeEach
    fun setUp() {
        root = Div()
        fieldset = Fieldset()
    }

    @Test
    @DisplayName("Create Fieldset with Legend")
    fun shouldCreateFieldsetWithLegend() {
        val expected = "legend"
        val fieldset = root.fieldset(expected)
        assertTrue(root.hasComponent(fieldset))
        assertFalse(fieldset.components.isEmpty())
        val legend = fieldset.components.first() as Legend
        assertEquals(expected, legend.text)
    }

    @Test
    @DisplayName("Create Fieldset without Legend")
    fun shouldCreateFieldsetWithoutLegend() {
        val fieldset = root.fieldset()
        assertTrue(root.hasComponent(fieldset))
        assertTrue(fieldset.components.isEmpty())
    }

    @Test
    @DisplayName("Create empty Legend")
    fun shouldCreateEmptyLegend() {
        val legend = fieldset.legend()
        assertEquals("", legend.text)
        assertTrue(fieldset.hasComponent(legend))
    }

    @Test
    @DisplayName("Create Legend with text")
    fun shouldCreateLegendWithText() {
        val text = "text"
        val legend = fieldset.legend(text)
        assertEquals(text, legend.text)
        assertTrue(fieldset.hasComponent(legend))
    }

    @Test
    @DisplayName("Create Legend with block")
    fun shouldCreateLegendWithBlock() {
        val expected = "name"
        val legend = fieldset.legend { name = expected }
        assertEquals(expected, legend.name)
        assertTrue(fieldset.hasComponent(legend))
    }

    @Test
    @DisplayName("Create Legend with text and block")
    fun shouldCreateLegendWithTextAndBlock() {
        val expected = "name"
        val text = "text"
        val legend = fieldset.legend(text) { name = expected }
        assertEquals(expected, legend.name)
        assertEquals(text, legend.text)
        assertTrue(fieldset.hasComponent(legend))
    }

}
