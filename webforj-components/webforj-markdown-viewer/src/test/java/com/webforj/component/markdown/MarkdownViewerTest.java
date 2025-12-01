package com.webforj.component.markdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class MarkdownViewerTest {

  MarkdownViewer component;

  @BeforeEach
  void setUp() {
    component = new MarkdownViewer();
  }

  @Nested
  @DisplayName("Constructors")
  class Constructors {

    @Test
    @DisplayName("should create MarkdownViewer with content")
    void shouldCreateMarkdownViewerWithContent() {
      MarkdownViewer viewer = new MarkdownViewer("# Hello World");

      assertEquals("# Hello World", viewer.getContent());
      assertFalse(viewer.isAutoScroll());
    }

    @Test
    @DisplayName("should create MarkdownViewer with default values")
    void shouldCreateMarkdownViewerWithDefaultValues() {
      MarkdownViewer viewer = new MarkdownViewer();

      assertEquals("", viewer.getContent());
      assertFalse(viewer.isAutoScroll());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    @DisplayName("should set and get properties")
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(MarkdownViewer.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Content API")
  class ContentApi {

    @Test
    @DisplayName("should append content")
    void shouldAppendContent() {
      component.setContent("Hello");
      component.append(" World");

      assertEquals("Hello World", component.getContent());
    }

    @Test
    @DisplayName("should append multiple chunks")
    void shouldAppendMultipleChunks() {
      component.append("# Title\n");
      component.append("Some ");
      component.append("content");

      assertEquals("# Title\nSome content", component.getContent());
    }

    @Test
    @DisplayName("should ignore null and empty appends")
    void shouldIgnoreNullAndEmptyAppends() {
      component.setContent("Hello");
      component.append(null);
      component.append("");

      assertEquals("Hello", component.getContent());
    }

    @Test
    @DisplayName("should clear content")
    void shouldClearContent() {
      component.setContent("# Hello World");
      component.clear();

      assertEquals("", component.getContent());
    }

    @Test
    @DisplayName("should handle null content")
    void shouldHandleNullContent() {
      component.setContent(null);

      assertEquals("", component.getContent());
    }
  }

  @Nested
  @DisplayName("Progressive Rendering API")
  class ProgressiveRenderingApi {

    @Test
    @DisplayName("should set rendering state when progressive render enabled and content appended")
    void shouldSetRenderingStateOnAppend() {
      component.setProgressiveRender(true);
      assertFalse(component.isRendering());

      component.append("Hello");

      assertTrue(component.isRendering());
    }

    @Test
    @DisplayName("should not set rendering state when progressive render is disabled")
    void shouldNotSetRenderingStateWhenDisabled() {
      component.setProgressiveRender(false);

      component.append("Hello");

      assertFalse(component.isRendering());
    }

    @Test
    @DisplayName("should set rendering state when progressive render is enabled and content is set")
    void shouldSetRenderingStateOnSetContent() {
      component.setProgressiveRender(true);

      component.setContent("Hello");

      assertTrue(component.isRendering());
    }

    @Test
    @DisplayName("should not set rendering state when content is empty")
    void shouldNotSetRenderingStateWhenContentEmpty() {
      component.setProgressiveRender(true);

      component.setContent("");

      assertFalse(component.isRendering());
    }

    @Test
    @DisplayName("should clear rendering state on clear")
    void shouldClearRenderingStateOnClear() {
      component.setProgressiveRender(true);
      component.append("Hello");
      assertTrue(component.isRendering());

      component.clear();

      assertFalse(component.isRendering());
    }

    @Test
    @DisplayName("should clear rendering state on stop")
    void shouldClearRenderingStateOnStop() {
      component.setProgressiveRender(true);
      component.append("Hello");
      assertTrue(component.isRendering());

      component.stop();

      assertFalse(component.isRendering());
    }

    @Test
    @DisplayName("should clear rendering state on flush")
    void shouldClearRenderingStateOnFlush() {
      component.setProgressiveRender(true);
      component.append("Hello");
      assertTrue(component.isRendering());

      component.flush();

      assertFalse(component.isRendering());
    }

    @Test
    @DisplayName("whenRenderComplete should return completed result when not rendering")
    void whenRenderCompleteShouldReturnCompletedResultWhenNotRendering() {
      component.setProgressiveRender(false);
      component.append("Hello");

      var result = component.whenRenderComplete();

      assertTrue(result.isDone());
    }

    @Test
    @DisplayName("whenRenderComplete should return pending result when rendering")
    void whenRenderCompleteShouldReturnPendingResultWhenRendering() {
      component.setProgressiveRender(true);
      component.append("Hello");

      var result = component.whenRenderComplete();

      assertFalse(result.isDone());
    }
  }

  @Nested
  @DisplayName("HasText API")
  class HasTextApi {

    @Test
    @DisplayName("setText should set content")
    void setTextShouldSetContent() {
      component.setText("# Hello");

      assertEquals("# Hello", component.getContent());
    }

    @Test
    @DisplayName("getText should return content")
    void getTextShouldReturnContent() {
      component.setContent("# Hello");

      assertEquals("# Hello", component.getText());
    }
  }
}
