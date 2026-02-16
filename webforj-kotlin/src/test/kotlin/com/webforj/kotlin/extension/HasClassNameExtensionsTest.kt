package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test

class HasClassNameExtensionsTest {
    val className = "test-class"
    val secondClass = "second-class"
    lateinit var component: Div

    @BeforeEach
    fun setUp() {
        component = Div()
        component.addClassName(className)
    }

    @AfterEach
    fun tearDown() {
        component.removeClassName(className)
        component.removeClassName(secondClass)
    }

    @Test
    @DisplayName("property classNames returns the component itself")
    fun shouldReturnThisWhenUsingClassNames() {
        val classNames = component.classNames
        assertNotNull(classNames)
        assertEquals(component, classNames)
    }

    @Test
    @DisplayName("plus operator adds class name")
    fun shouldAddClassWithPlus() {
        component.classNames + secondClass
    }

    @Test
    @DisplayName("plus operator can be chained")
    fun shouldChainPlus() {
        val thirdClass = "third-class"
        component.classNames + secondClass + thirdClass
        component.removeClassName(thirdClass)
    }

    @Test
    @DisplayName("plusAssign operator adds class name")
    fun shouldAddClassWithPlusAssign() {
        component.classNames += secondClass
    }

    @Test
    @DisplayName("minus operator removes class name")
    fun shouldRemoveClassWithMinus() {
        component.classNames - className
    }

    @Test
    @DisplayName("minus operator can be chained")
    fun shouldChainMinus() {
        component.addClassName(secondClass)
        component.classNames - className - secondClass
    }

    @Test
    @DisplayName("minusAssign operator removes class name")
    fun shouldRemoveClassWithMinusAssign() {
        component.classNames -= className
    }

}
