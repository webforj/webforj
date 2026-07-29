package com.webforj.devtools.craftforj.inspector.contribution.appearance.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppLayoutFooterShadowContributionTest {

  private final AppLayoutFooterShadowContribution contribution =
      new AppLayoutFooterShadowContribution();

  @Test
  void shouldGet() {
    AppLayout component = mock(AppLayout.class);
    when(component.getFooterShadow()).thenReturn(AppLayout.Shadow.SCROLL);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("FooterShadow", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(AppLayout.Shadow.class.getCanonicalName() + ".SCROLL", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppLayout component = mock(AppLayout.class);

    assertTrue(contribution.set(component, AppLayout.Shadow.class.getCanonicalName() + ".ALWAYS"));
    verify(component).setFooterShadow(AppLayout.Shadow.ALWAYS);
  }
}
