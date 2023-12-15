package org.dwcj.annotation;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import org.dwcj.App;
import org.dwcj.Page;
import org.dwcj.exceptions.DwcjException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AnnotationProcessorTest {

  @Test
  void testProcessAppTitle() {
    @AppTitle(value = "Test App", format = "{BrowserTitle} - Generated")
    class MockAppClass extends App {
      @Override
      public void run() throws DwcjException {
        // pass
      }
    }

    try (MockedStatic<App> mockedApp = mockStatic(App.class)) {
      Page mockPage = mock(Page.class);
      mockedApp.when(App::getPage).thenReturn(mockPage);

      AnnotationProcessor processor = new AnnotationProcessor();
      MockAppClass mockAppClass = new MockAppClass();

      processor.processAppAnnotations(mockAppClass, AnnotationProcessor.RunningPhase.POST_RUN);
      verify(mockPage).setTitle("Test App", "{BrowserTitle} - Generated");
    }
  }
}
