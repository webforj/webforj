package com.webforj.component.markdown;

import com.webforj.PendingResult;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A component for rendering markdown content.
 *
 * <p>
 * The MarkdownViewer component renders markdown text as HTML. It supports standard markdown syntax
 * including headers, lists, code blocks, links, images, and more. The component also supports emoji
 * rendering and syntax highlighting for code blocks when <code>Prism.js</code> is available.
 * </p>
 *
 * <p>
 * The component supports progressive rendering for streaming scenarios like AI chat interfaces,
 * where content arrives in chunks and is displayed incrementally.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
@NodeName("dwc-markdown-viewer")
public class MarkdownViewer extends ElementComposite
    implements HasClassName<MarkdownViewer>, HasStyle<MarkdownViewer>,
    HasVisibility<MarkdownViewer>, HasSize<MarkdownViewer>, HasText<MarkdownViewer> {

  // Properties
  private final PropertyDescriptor<String> contentProp = PropertyDescriptor.property("content", "");
  private final PropertyDescriptor<Boolean> autoScrollProp =
      PropertyDescriptor.property("autoScroll", false);
  private final PropertyDescriptor<Boolean> progressiveRenderProp =
      PropertyDescriptor.property("progressiveRender", false);
  private final PropertyDescriptor<Integer> renderSpeedProp =
      PropertyDescriptor.property("renderSpeed", 4);

  private final StringBuffer contentBuffer = new StringBuffer();
  private final List<PendingResult<MarkdownViewer>> whenRenderCompleteResults =
      new CopyOnWriteArrayList<>();
  private volatile boolean rendering = false;

  /**
   * Creates a new MarkdownViewer component with the given content.
   *
   * @param content the markdown content to render
   */
  public MarkdownViewer(String content) {
    super();
    setContent(content);
    // Listen for client-side progressive render completion
    getElement().addEventListener("dwc-progressive-render-complete", e -> {
      completeRendering();
    });
  }

  /**
   * Creates a new MarkdownViewer component.
   */
  public MarkdownViewer() {
    this("");
  }

  /**
   * Sets whether to automatically scroll to the bottom when content changes.
   *
   * <p>
   * When enabled, the component will automatically scroll to show the latest content as it is
   * added. This is useful for streaming content or chat-like interfaces. The auto-scroll behavior
   * is paused when the user manually scrolls up, and resumes when they scroll back to the bottom.
   * </p>
   *
   * @param autoScroll true to enable auto-scroll, false to disable
   * @return the component itself
   */
  public MarkdownViewer setAutoScroll(boolean autoScroll) {
    set(autoScrollProp, autoScroll);
    return this;
  }

  /**
   * Checks if auto-scroll is enabled.
   *
   * @return true if auto-scroll is enabled, false otherwise
   */
  public boolean isAutoScroll() {
    return get(autoScrollProp);
  }

  /**
   * Sets whether to enable progressive rendering of content.
   *
   * <p>
   * When enabled, content is rendered progressively character-by-character rather than all at once.
   * This is useful for AI chat interfaces where the server streams responses in chunks. The
   * client-side render speed can be adjusted with {@link #setRenderSpeed(int)} to match or exceed
   * the server's streaming rate.
   * </p>
   *
   * @param progressiveRender true to enable progressive rendering, false to render immediately
   * @return the component itself
   */
  public MarkdownViewer setProgressiveRender(boolean progressiveRender) {
    set(progressiveRenderProp, progressiveRender);
    return this;
  }

  /**
   * Checks if progressive rendering is enabled.
   *
   * @return true if progressive rendering is enabled, false otherwise
   */
  public boolean isProgressiveRender() {
    return get(progressiveRenderProp);
  }

  /**
   * Sets the render speed for progressive rendering.
   *
   * <p>
   * Controls how many characters are rendered per frame when {@link #setProgressiveRender(boolean)}
   * is enabled. Higher values result in faster rendering. Adjust this based on the server's
   * streaming speed. At 60fps: 4 = 240 chars/sec, 6 = 360 chars/sec, 10 = 600 chars/sec.
   * </p>
   *
   * @param speed the number of characters to render per frame (default is 4)
   * @return the component itself
   */
  public MarkdownViewer setRenderSpeed(int speed) {
    set(renderSpeedProp, speed);
    return this;
  }

  /**
   * Gets the render speed for progressive rendering.
   *
   * @return the number of characters rendered per frame
   */
  public int getRenderSpeed() {
    return get(renderSpeedProp);
  }

  /**
   * Sets the markdown content to render.
   *
   * <p>
   * This replaces any existing content. For appending content, use {@link #append(String)}.
   * </p>
   *
   * @param content the markdown content
   * @return the component itself
   */
  public MarkdownViewer setContent(String content) {
    contentBuffer.setLength(0);
    contentBuffer.append(content != null ? content : "");
    set(contentProp, contentBuffer.toString());

    if (isProgressiveRender() && !contentBuffer.isEmpty()) {
      rendering = true;
    }

    return this;
  }

  /**
   * Gets the markdown content.
   *
   * @return the markdown content
   */
  public String getContent() {
    return get(contentProp);
  }

  /**
   * Appends a chunk of markdown content to the existing content.
   *
   * <p>
   * This method is useful for streaming scenarios where content is received in chunks.
   * </p>
   *
   * @param chunk the markdown chunk to append
   * @return the component itself
   */
  public MarkdownViewer append(String chunk) {
    if (chunk != null && !chunk.isEmpty()) {
      contentBuffer.append(chunk);
      set(contentProp, contentBuffer.toString());

      if (isProgressiveRender()) {
        rendering = true;
      }
    }

    return this;
  }

  /**
   * Clears all markdown content.
   *
   * @return the component itself
   */
  public MarkdownViewer clear() {
    contentBuffer.setLength(0);
    set(contentProp, "");
    completeRendering();
    return this;
  }

  /**
   * Alias for {@link #setContent(String)}.
   *
   * @param text the markdown content
   * @return the component itself
   */
  @Override
  public MarkdownViewer setText(String text) {
    return setContent(text);
  }

  /**
   * Alias for {@link #getContent()}.
   *
   * @return the markdown content
   */
  @Override
  public String getText() {
    return getContent();
  }

  /**
   * Checks if the component is currently progressively rendering content.
   *
   * <p>
   * Returns {@code true} when progressive rendering is enabled and the component has not yet
   * finished displaying all content. This can be used to determine when to show or hide loading
   * indicators or stop buttons in chat interfaces.
   * </p>
   *
   * @return {@code true} if progressive rendering is in progress, {@code false} otherwise
   */
  public boolean isRendering() {
    return rendering;
  }

  /**
   * Stops progressive rendering, keeping content as currently displayed.
   *
   * <p>
   * This method is typically called when the user cancels streaming. Any remaining buffered content
   * that has not yet been displayed will be discarded.
   * </p>
   *
   * @return the component itself
   */
  public MarkdownViewer stop() {
    getElement().callJsFunctionVoidAsync("stop");
    completeRendering();
    return this;
  }

  /**
   * Stops progressive rendering and immediately displays all buffered content.
   *
   * <p>
   * This method is typically called when streaming completes to immediately show all remaining
   * content without waiting for progressive rendering to finish.
   * </p>
   *
   * @return the component itself
   */
  public MarkdownViewer flush() {
    getElement().callJsFunctionVoidAsync("flush");
    completeRendering();
    return this;
  }

  /**
   * Returns a {@link PendingResult} that completes when progressive rendering finishes.
   *
   * <p>
   * This is useful for knowing when all content has been displayed to the user after streaming
   * completes. If progressive rendering is not active or no content is being rendered, the
   * PendingResult will immediately complete.
   * </p>
   *
   * @return a PendingResult that completes when rendering is done
   */
  public PendingResult<MarkdownViewer> whenRenderComplete() {
    if (!rendering) {
      return PendingResult.completedWith(this);
    }

    PendingResult<MarkdownViewer> result = new PendingResult<>();
    whenRenderCompleteResults.add(result);
    return result;
  }

  private void completeRendering() {
    rendering = false;
    whenRenderCompleteResults.forEach(r -> r.complete(this));
    whenRenderCompleteResults.clear();
  }
}
