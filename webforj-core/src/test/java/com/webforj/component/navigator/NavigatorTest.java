package com.webforj.component.navigator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.sysgui.BBjNavigator;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.navigator.event.NavigatorChangeEvent;
import com.webforj.component.navigator.event.NavigatorMoveFirstEvent;
import com.webforj.component.navigator.event.NavigatorMoveLastEvent;
import com.webforj.component.navigator.event.NavigatorMoveNextEvent;
import com.webforj.component.navigator.event.NavigatorMovePreviousEvent;
import com.webforj.data.Paginator;
import com.webforj.data.repository.CollectionRepository;
import com.webforj.data.repository.Repository;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;


@ExtendWith(MockitoExtension.class)
class NavigatorTest {

  @Mock
  BBjNavigator control;

  @InjectMocks
  Navigator component;

  private static Stream<Arguments> provideConstructors() {
    Repository<?> repo = new CollectionRepository<>(List.of(1, 2, 3));

    return Stream.of(
        Arguments.of(new Navigator(100), 100, Paginator.DEFAULT_PAGE_SIZE,
            Navigator.Layout.PREVIEW),
        Arguments.of(new Navigator(100, 10), 100, 10, Navigator.Layout.PREVIEW),
        Arguments.of(new Navigator(100, Navigator.Layout.PAGES), 100, Paginator.DEFAULT_PAGE_SIZE,
            Navigator.Layout.PAGES),
        Arguments.of(new Navigator(100, 10, Navigator.Layout.PAGES), 100, 10,
            Navigator.Layout.PAGES),
        Arguments.of(new Navigator(repo), repo.size(), Paginator.DEFAULT_PAGE_SIZE,
            Navigator.Layout.PREVIEW),
        Arguments.of(new Navigator(repo, 10), repo.size(), 10, Navigator.Layout.PREVIEW),
        Arguments.of(new Navigator(repo, Navigator.Layout.PAGES), repo.size(),
            Paginator.DEFAULT_PAGE_SIZE, Navigator.Layout.PAGES),
        Arguments.of(new Navigator(repo, 10, Navigator.Layout.PAGES), repo.size(), 10,
            Navigator.Layout.PAGES));
  }

  @ParameterizedTest
  @MethodSource("provideConstructors")
  void shouldCorrectlyBeInitialized(Navigator navigator, int expectedTotalItems,
      int expectedPageSize, Navigator.Layout expectedLayout) {
    Paginator paginator = navigator.getPaginator();
    assertNotNull(paginator);
    assertEquals(expectedTotalItems, paginator.getTotalItems());
    assertEquals(expectedPageSize, paginator.getSize());
    assertEquals(expectedLayout, navigator.getLayout());
  }

  @Test
  void shouldSetGetAutoDisable() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setAutoDisable(true);
    assertTrue(component.isAutoDisable());

    assertEquals(true, component.getProperty("autoDisable"));
  }

  @Test
  void shouldSetGetLayout() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setLayout(Navigator.Layout.NONE);
    assertEquals(Navigator.Layout.NONE, component.getLayout());

    assertEquals("none", component.getProperty("layout"));
  }

  @Test
  void shouldHideShowMainButtons() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setHideMainButtons(true);
    assertTrue(component.isHideMainButtons());

    assertFalse(component.isVisible(Navigator.Part.FIRST_BUTTON));
    assertFalse(component.isVisible(Navigator.Part.PREVIOUS_BUTTON));
    assertFalse(component.isVisible(Navigator.Part.NEXT_BUTTON));
    assertFalse(component.isVisible(Navigator.Part.LAST_BUTTON));

    component.setHideMainButtons(false);
    assertFalse(component.isHideMainButtons());

    assertTrue(component.isVisible(Navigator.Part.FIRST_BUTTON));
    assertTrue(component.isVisible(Navigator.Part.PREVIOUS_BUTTON));
    assertTrue(component.isVisible(Navigator.Part.NEXT_BUTTON));
    assertTrue(component.isVisible(Navigator.Part.LAST_BUTTON));
  }

  @ParameterizedTest
  @EnumSource(Navigator.Part.class)
  void shouldSetGetTextPart(Navigator.Part part) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    String val = "part-text";
    component.setText(val, part);
    assertEquals(val, component.getText(part));
  }

  @Test
  void shouldSetGetLabel() {
    String val = "label-text";
    component.setLabel(val);
    assertEquals(val, component.getLabel());
  }

  @ParameterizedTest
  @EnumSource(Navigator.Part.class)
  void shouldSetGetTooltipTextPart(Navigator.Part part) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    String val = "part-tooltip-text";
    component.setTooltipText(val, part);
    assertEquals(val, component.getTooltipText(part));
  }

  @ParameterizedTest
  @EnumSource(Navigator.Part.class)
  void shouldHideShowParts(Navigator.Part part) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setVisible(false, part);
    assertEquals(false, component.isVisible(part),
        String.format("Part %s should be hidden", part.name()));
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldFireChangeEvent() {
      NavigatorChangeEvent[] changeEvent = new NavigatorChangeEvent[1];
      NavigatorMoveNextEvent nextEvent = new NavigatorMoveNextEvent(component,
          Map.of("current", 2, "startIndex", 0, "endIndex", 10));

      component.onChange(e -> changeEvent[0] = e);
      component.handleChangeEvent(nextEvent);

      assertNotNull(changeEvent[0]);
      assertEquals(NavigatorChangeEvent.Direction.NEXT, changeEvent[0].getDirection());
      assertEquals(2, changeEvent[0].getCurrent());
      assertEquals(0, changeEvent[0].getStartIndex());
      assertEquals(10, changeEvent[0].getEndIndex());
    }

    @Test
    void shouldAddSupportedEvents() {
      component.onMoveFirst(e -> {
      });
      assertEquals(2, component.getEventListeners(NavigatorMoveFirstEvent.class).size());

      component.onMoveLast(e -> {
      });
      assertEquals(2, component.getEventListeners(NavigatorMoveLastEvent.class).size());

      component.onMoveNext(e -> {
      });
      assertEquals(2, component.getEventListeners(NavigatorMoveNextEvent.class).size());

      component.onMovePrevious(e -> {
      });
      assertEquals(2, component.getEventListeners(NavigatorMovePreviousEvent.class).size());

      component.onChange(e -> {
      });
      assertEquals(1, component.getEventListeners(NavigatorChangeEvent.class).size());
    }
  }
}
