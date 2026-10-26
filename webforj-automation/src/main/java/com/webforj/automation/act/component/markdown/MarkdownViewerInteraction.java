package com.webforj.automation.act.component.markdown;

import com.webforj.automation.act.UserInteraction;
import com.webforj.component.markdown.MarkdownViewer;
import java.util.HashMap;
import java.util.function.Consumer;

/**
 * Reports what the browser reports about a markdown viewer on its own.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class MarkdownViewerInteraction {
  private static final String RENDER_COMPLETE = "dwc-progressive-render-complete";

  private MarkdownViewerInteraction() {}

  /**
   * Returns the interaction that reports a markdown viewer finished rendering its content
   * progressively. The viewer stops rendering and completes every result of
   * {@link MarkdownViewer#whenRenderComplete()}.
   *
   * <pre>{@code
   * UserInteraction.perform(viewer, MarkdownViewerInteraction.completeRendering())
   * }</pre>
   *
   * @return the interaction to perform on the markdown viewer
   */
  public static Consumer<MarkdownViewer> completeRendering() {
    return viewer -> UserInteraction.fireEvent(viewer, RENDER_COMPLETE, new HashMap<>());
  }
}
