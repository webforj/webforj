package com.webforj.devtools.craftforj.inspector.contribution.layout.splitter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SplitterOrientationContributionTest {

  private final SplitterOrientationContribution contribution =
      new SplitterOrientationContribution();

  @Test
  void shouldGet() {
    Splitter splitter = mock(Splitter.class);
    when(splitter.getOrientation()).thenReturn(Splitter.Orientation.VERTICAL);

    var result = contribution.get(splitter);

    assertTrue(result.isPresent());
    assertEquals("Orientation", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.layout.splitter.Splitter.Orientation.VERTICAL",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    Splitter splitter = mock(Splitter.class);

    boolean success = contribution.set(splitter, Splitter.Orientation.HORIZONTAL);

    assertTrue(success);
    verify(splitter).setOrientation(Splitter.Orientation.HORIZONTAL);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Splitter splitter = mock(Splitter.class);
    assertEquals(Splitter.Orientation.class, contribution.findEnumClass(splitter));
  }
}
