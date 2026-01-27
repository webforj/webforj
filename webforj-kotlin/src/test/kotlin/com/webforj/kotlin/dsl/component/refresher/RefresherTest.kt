package com.webforj.kotlin.dsl.component.refresher

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

class RefresherTest {
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
    @DisplayName("Create empty Refresher")
    fun shouldCreateEmptyRefresher() {
        val refresher = root.refresher()
        assertNotNull(refresher)
        assertTrue(root.hasComponent(refresher))
    }

    @Test
    @DisplayName("Create Refresher with configuration block")
    fun shouldCreateRefresherWithBlock() {
        val blockExecuted = AtomicBoolean(false)
        val customThreshold = 75
        
        val refresher = root.refresher {
            blockExecuted.set(true)
            threshold = customThreshold
        }
        
        assertNotNull(refresher)
        assertTrue(root.hasComponent(refresher))
        assertTrue(blockExecuted.get())
        assertEquals(customThreshold, refresher.threshold)
    }
}