package com.webforj.component.refresher;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.icons.FeatherIcon;
import com.webforj.component.refresher.event.RefresherRefreshEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class RefresherTest {

  Refresher component;

  @BeforeEach
  void setUp() {
    component = new Refresher();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Refresher.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldSetArrowIconFromIconComponent() {
      component.setArrowIcon(FeatherIcon.LOADER.create());
      assertEquals("feather:loader", component.getArrowIcon());
    }

    @Test
    void shouldSetRefreshIconFromIconComponent() {
      component.setRefreshIcon(FeatherIcon.LOADER.create());
      assertEquals("feather:loader", component.getRefreshIcon());
    }

    @Test
    void shouldEnableAndDisable() {
      component.setEnabled(false);
      assertFalse(component.isEnabled());
      component.setEnabled(true);
      assertTrue(component.isEnabled());
    }

    @Test
    void shouldSetGetI18n() {
      RefresherI18n i18n = new RefresherI18n();
      i18n.setPull("Pull");
      i18n.setRelease("Release");
      i18n.setRefresh("Refresh");
      component.setI18n(i18n);
      assertEquals(i18n, component.getI18n());

      assertEquals("Pull", component.getOriginalElement().getProperty("textPull"));
      assertEquals("Release", component.getOriginalElement().getProperty("textRelease"));
      assertEquals("Refresh", component.getOriginalElement().getProperty("textRefresh"));
    }
  }


  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddScrollListener() {
      component.onRefresh(event -> {
      });

      List<EventListener<RefresherRefreshEvent>> listeners =
          component.getEventListeners(RefresherRefreshEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<RefresherRefreshEvent>);
    }
  }
}
