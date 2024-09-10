package com.webforj.annotation;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.webforj.App;
import com.webforj.Page;
import com.webforj.exceptions.WebforjException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AnnotationProcessorTest {
  MockedStatic<Page> mockedPage;
  Page page;

  @BeforeEach
  void setup() {
    mockedPage = mockStatic(Page.class);
    page = mock(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
  }

  @AfterEach
  void tearDown() {
    mockedPage.close();
  }

  @Test
  void testProcessAppTitle() {
    @AppTitle(value = "Test App", format = "{BrowserTitle} - Generated")
    class MockAppClass extends App {
      @Override
      public void run() throws WebforjException {
        // pass
      }
    }

    AnnotationProcessor processor = new AnnotationProcessor();
    MockAppClass mockAppClass = new MockAppClass();

    processor.processAppAnnotations(mockAppClass);
    verify(page).setTitle("Test App", "{BrowserTitle} - Generated");
  }
}
