package com.webforj.automation.act.component.markdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.markdown.MarkdownViewer;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class MarkdownViewerInteractionTest {

  @Test
  void shouldCompleteTheRenderResultsOnlyWhenTheViewerReportsTheEndOfRendering() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      MarkdownViewer viewer = new MarkdownViewer().setProgressiveRender(true);
      frame.add(viewer);
      viewer.setContent("# Orders");
      List<String> rendered = new ArrayList<>();
      PendingResult<MarkdownViewer> result = viewer.whenRenderComplete();
      result.thenAccept(completed -> rendered.add(completed.getContent()));
      boolean doneBefore = result.isDone();

      UserInteraction.perform(viewer, MarkdownViewerInteraction.completeRendering());

      assertFalse(doneBefore);
      assertTrue(result.isDone());
      assertFalse(viewer.isRendering());
      assertEquals(List.of("# Orders"), rendered);
    }
  }
}
