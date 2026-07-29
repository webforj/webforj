package com.webforj.devtools.craftforj.inspector.contribution.layout.splitter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SplitterPositionRelativeContributionTest {

  private final SplitterPositionRelativeContribution contribution =
      new SplitterPositionRelativeContribution();

  @Test
  void shouldGet() {
    Splitter splitter = mock(Splitter.class);
    when(splitter.getPositionRelative()).thenReturn(75.0);

    var result = contribution.get(splitter);

    assertTrue(result.isPresent());
    assertEquals("PositionRelative", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(75.0, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Splitter splitter = mock(Splitter.class);

    boolean success = contribution.set(splitter, 30.0);

    assertTrue(success);
    verify(splitter).setPositionRelative(30.0);
  }

}
