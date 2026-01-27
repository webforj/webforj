package com.webforj.kotlin.dsl.component.markdown

import com.webforj.component.html.elements.Div
import com.webforj.component.markdown.MarkdownViewer
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.markdown.markdownViewer
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicBoolean

class MarkdownViewerTest {
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
    @DisplayName("Create empty MarkdownViewer")
    fun shouldCreateEmptyMarkdownViewer() {
        val markdownViewer = root.markdownViewer()
        Assertions.assertNotNull(markdownViewer)
        Assertions.assertTrue(root.hasComponent(markdownViewer))
        Assertions.assertTrue(markdownViewer.content.isEmpty() || markdownViewer.content == null)
    }

    @Test
    @DisplayName("Create MarkdownViewer with simple content")
    fun shouldCreateMarkdownViewerWithSimpleContent() {
        val content = "# Hello World"
        val markdownViewer = root.markdownViewer(content)
        
        Assertions.assertNotNull(markdownViewer)
        Assertions.assertTrue(root.hasComponent(markdownViewer))
        Assertions.assertEquals(content, markdownViewer.content)
    }

    @Test
    @DisplayName("Create MarkdownViewer with configuration block")
    fun shouldCreateMarkdownViewerWithBlock() {
        val blockExecuted = AtomicBoolean(false)
        
        val markdownViewer = root.markdownViewer {
            blockExecuted.set(true)
        }
        
        Assertions.assertNotNull(markdownViewer)
        Assertions.assertTrue(root.hasComponent(markdownViewer))
        Assertions.assertTrue(blockExecuted.get())
    }

    @Test
    @DisplayName("Create MarkdownViewer with content and configuration block")
    fun shouldCreateMarkdownViewerWithContentAndBlock() {
        val content = "## Sample Content"
        val blockExecuted = AtomicBoolean(false)
        val modifiedContent = "## Modified Content"
        
        val markdownViewer = root.markdownViewer(content) {
            blockExecuted.set(true)
            this.content = modifiedContent
        }
        
        Assertions.assertNotNull(markdownViewer)
        Assertions.assertTrue(root.hasComponent(markdownViewer))
        Assertions.assertTrue(blockExecuted.get())
        Assertions.assertEquals(modifiedContent, markdownViewer.content)
    }

}