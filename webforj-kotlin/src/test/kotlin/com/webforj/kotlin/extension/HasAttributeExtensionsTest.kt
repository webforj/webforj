package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test

class HasAttributeExtensionsTest {
    val attribute = "attribute"
    val value = "value"
    lateinit var component: Div

    @BeforeEach
    fun setUp() {
        component = Div()
        component.setAttribute(attribute, value)
    }

    @AfterEach
    fun tearDown() {
        component.removeAttribute(attribute)
    }

    @Test
    @DisplayName("property attributes returns the component it self")
    fun shouldReturnThisThenUsingAttributes() {
        val attributes = component.attributes
        assertNotNull(attributes)
        assertEquals(component, attributes)
    }

    @Test
    @DisplayName("get operator returns attribute")
    fun shouldRetrieveAttribute() {
        assertEquals(value, component.attributes[attribute])
    }

    @Test
    @DisplayName("get operator returns null for missing attribute")
    fun shouldReturnNullForMissingAttribute() {
        assertNull(component.attributes["missing"])
    }

    @Test
    @DisplayName("set operator updates attributes")
    fun shouldUpdateAttributes() {
        val component = Div()
        assertNull(component.getAttribute(attribute))
        component.attributes[attribute] = value
        assertEquals(value, component.getAttribute(attribute))
    }

    @Test
    @DisplayName("minus operator removes attribute")
    fun shouldRemoveAttributeWithMinus() {
        assertEquals(value, component.getAttribute(attribute))
        component.attributes - attribute
        assertNull(component.getAttribute(attribute))
    }

    @Test
    @DisplayName("minus operator can be chained")
    fun shouldChainMinus() {
        assertEquals(value, component.getAttribute(attribute))
        val key = "key"
        component.setAttribute(key, value)
        component.attributes - attribute - key
        assertNull(component.getAttribute(attribute))
        assertNull(component.getAttribute(key))
    }

    @Test
    @DisplayName("minusAssign operator removes attribute")
    fun minusAssign() {
        assertEquals(value, component.getAttribute(attribute))
        component.attributes -= attribute
        assertNull(component.getAttribute(attribute))
    }

}