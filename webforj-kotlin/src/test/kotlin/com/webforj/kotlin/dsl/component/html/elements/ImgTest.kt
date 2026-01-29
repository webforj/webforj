package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test

class ImgTest {
    lateinit var root: Div

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @Test
    @DisplayName("Create Img with source")
    fun shouldCreateImgWithSrc() {
        val src = "src"
        val img = root.img(src)
        assertEquals(src, img.src)
        assertEquals("", img.alt)
        assertTrue(root.hasComponent(img))
    }

    @Test
    @DisplayName("Create Img with alt text")
    fun shouldCreateImgWithAlt() {
        val alt = "alt"
        val img = root.img(alt = alt)
        assertEquals("", img.src)
        assertEquals(alt, img.alt)
        assertTrue(root.hasComponent(img))
    }

    @Test
    @DisplayName("Create Img with source and alt text")
    fun shouldCreateImgWithSrcAndAlt() {
        val src = "src"
        val alt = "alt"
        val img = root.img(src, alt)
        assertEquals(src, img.src)
        assertEquals(alt, img.alt)
        assertTrue(root.hasComponent(img))
    }

}
