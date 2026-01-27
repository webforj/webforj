package com.webforj.kotlin.dsl.component.infiniitescroll

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class InfiniteScrollTest {
    lateinit var root: HasComponents

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @AfterEach
    fun tearDown() {
        root.removeAll()
    }

    @Test
    @DisplayName("Create empty InfiniteScroll")
    fun shouldCreateEmptyInfiniteScroll() {
        val infiniteScroll = root.infiniteScroll()
        assertNotNull(infiniteScroll)
        assertTrue(root.hasComponent(infiniteScroll))
        assertEquals("Loading data", infiniteScroll.text)
    }

    @Test
    @DisplayName("Create InfiniteScroll with loading text")
    fun shouldCreateInfiniteScrollWithText() {
        val expectedText = "Loading more items..."
        val infiniteScroll = root.infiniteScroll(expectedText)
        
        assertNotNull(infiniteScroll)
        assertTrue(root.hasComponent(infiniteScroll))
        assertEquals(expectedText, infiniteScroll.text)
    }

    @Test
    @DisplayName("Create InfiniteScroll with configuration block")
    fun shouldCreateInfiniteScrollWithBlock() {
        val blockExecuted = AtomicBoolean(false)
        val customText = "Custom loading message"
        
        val infiniteScroll = root.infiniteScroll {
            blockExecuted.set(true)
            text = customText
        }
        
        assertNotNull(infiniteScroll)
        assertTrue(root.hasComponent(infiniteScroll))
        assertTrue(blockExecuted.get())
        assertEquals(customText, infiniteScroll.text)
    }

}