package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.layout.appnav.event.AppNavLocationChangedEvent;
import com.webforj.component.layout.appnav.event.AppNavPinEvent;
import com.webforj.component.layout.appnav.event.AppNavSearchEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AppNavTest {

  AppNav component;

  @BeforeEach
  void setUp() {
    component = new AppNav();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(AppNav.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddNavigateListener() {
      component.onLocationChanged(event -> {
      });

      List<EventListener<AppNavLocationChangedEvent>> listeners =
          component.getEventListeners(AppNavLocationChangedEvent.class);

      assertEquals(2, listeners.size());
      assertTrue(listeners.get(1) instanceof EventListener<AppNavLocationChangedEvent>);
    }

    @Test
    void shouldAddSearchListener() {
      component.onSearch(event -> {
      });

      List<EventListener<AppNavSearchEvent>> listeners =
          component.getEventListeners(AppNavSearchEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AppNavSearchEvent>);
    }

    @Test
    void shouldAddPinListener() {
      component.onPin(event -> {
      });

      List<EventListener<AppNavPinEvent>> listeners =
          component.getEventListeners(AppNavPinEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AppNavPinEvent>);
    }
  }

  @Nested
  @DisplayName("Pinning API")
  class PinningApi {

    @Test
    void shouldConfigurePinning() {
      component.getPinning().setEnabled(true).setAutosave(false).setTitle("Pinned");

      assertTrue(component.getPinning().isEnabled());
      assertEquals(false, component.getPinning().isAutosave());
      assertEquals("Pinned", component.getPinning().getTitle());
    }

    @Test
    void shouldReturnPinnedItemsAcrossTheTree() {
      AppNavItem home = new AppNavItem("Home", "/home");
      AppNavItem group = new AppNavItem("Docs");
      AppNavItem reports = new AppNavItem("Reports", "/docs/reports").setPinned(true);
      group.addItem(reports);
      component.addItem(home);
      component.addItem(group);

      assertEquals(List.of(reports), component.getPinning().getItems());

      home.setPinned(true);
      assertEquals(List.of(home, reports), component.getPinning().getItems());
    }

    @Test
    void shouldConfigurePinIcons() {
      component.getPinning().setUnpinnedIcon(icon("tabler", "bookmark"))
          .setPinnedIcon(icon("tabler", "bookmark-filled"));

      assertEquals("tabler:bookmark", component.getPinning().getUnpinnedIcon());
      assertEquals("tabler:bookmark-filled", component.getPinning().getPinnedIcon());
    }

    private IconDefinition<Object> icon(String pool, String name) {
      return new IconDefinition<>() {
        @Override
        public Object setName(String value) {
          return this;
        }

        @Override
        public String getName() {
          return name;
        }

        @Override
        public Object setPool(String value) {
          return this;
        }

        @Override
        public String getPool() {
          return pool;
        }
      };
    }
  }

  @Nested
  @DisplayName("Search API")
  class SearchApi {

    @Test
    void shouldKeepPlainTextEmptyMessage() {
      component.getSearch().setEmptyMessage("Nothing found");

      assertEquals("Nothing found", component.getSearch().getEmptyMessage());
    }

    @Test
    void shouldSanitizeTagsFromPlainTextEmptyMessage() {
      component.getSearch().setEmptyMessage("Nothing <b>found</b>");

      assertEquals("Nothing found", component.getSearch().getEmptyMessage());
    }

    @Test
    void shouldKeepHtmlWrappedEmptyMessage() {
      component.getSearch().setEmptyMessage("<html><strong>Nothing</strong></html>");

      assertEquals("<html><strong>Nothing</strong></html>",
          component.getSearch().getEmptyMessage());
    }
  }
}
