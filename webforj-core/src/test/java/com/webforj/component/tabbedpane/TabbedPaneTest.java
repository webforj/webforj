package com.webforj.component.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.startup.type.BBjException;
import com.webforj.component.DwcComponentMock;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.tabbedpane.event.TabCloseEvent;
import com.webforj.component.tabbedpane.event.TabDeselectEvent;
import com.webforj.component.tabbedpane.event.TabSelectEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TabbedPaneTest {

  @Mock
  BBjTabCtrl control;

  @InjectMocks
  TabbedPane component = new TabbedPane();

  @Nested
  @DisplayName("Add Tabs without Associated Component API")
  class AddTabsWithoutAssociatedComponentApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldAddTabs(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      Tab addedTab = component.addTab("Tab1");

      assertEquals(component.getTabs().get(0), addedTab);

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).addTab(addedTab.getText(), -1);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldInsertTabs(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      component.addTab("Tab1");
      Tab insertedTab = component.insertTab(0, "Tab2");

      assertEquals(component.getTabs().get(0), insertedTab);

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).insertTab(0, insertedTab.getText(), -1);
    }

    @Test
    void shouldCatchupWithTabConfigurationsAfterAdding() throws BBjException {
      TabbedPane spy = spy(component);
      when(spy.isAttached()).thenReturn(true);

      DwcComponentMock panel = new DwcComponentMock();
      Tab addedTab = spy.addTab("Tab1", panel);

      assertEquals(spy.getTabs().get(0), addedTab);

      addedTab.setEnabled(false);
      addedTab.setClosable(true);
      addedTab.setTooltip("Tooltip");
      addedTab.setText("New Tab");

      verify(control, times(1)).addTab("Tab1", panel.getControl());
      verify(control, times(1)).setTitleAt(0, "New Tab");
      verify(control, times(1)).setEnabledAt(0, false);
      verify(control, times(1)).setCloseableAt(0, true);
      verify(control, times(1)).setToolTipTextAt(0, "Tooltip");
    }

    @Test
    void shouldThrowDwcjRuntimeException() throws BBjException {
      doThrow(BBjException.class).when(control).addTab(anyString(), anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.addTab("Tab1"));
    }

    @Test
    void shouldThrowsNullPointerException() {
      assertThrows(NullPointerException.class, () -> component.addTab((Tab) null));
    }

    @Test
    void shouldThrowIllegalArgumentException() {
      Tab tab = new Tab("Tab1");
      component.addTab(tab);
      assertThrows(IllegalArgumentException.class, () -> component.addTab(tab));
    }

    @Test
    void onAttachWillCatchWithAddedTabs() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      DwcComponentMock panel = new DwcComponentMock();
      Tab tab = component.addTab("Tab1", panel);
      tab.setEnabled(false);
      tab.setClosable(true);
      tab.setText("New Tab");

      verify(control, times(0)).addTab(tab.getText(), panel.getControl());
      verify(control, times(0)).setEnabledAt(0, false);
      verify(control, times(0)).setCloseableAt(0, true);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).addTab(tab.getText(), panel.getControl());
      verify(control, times(1)).setEnabledAt(0, false);
      verify(control, times(1)).setCloseableAt(0, true);
    }
  }

  @Nested
  @DisplayName("Add Tabs with Associated Component API")
  class AddTabsWithAssociatedComponentApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldAddTabs(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      Tab addedTab = component.addTab("Tab1", panel);

      assertEquals(component.getTabs().get(0), addedTab);

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).addTab(addedTab.getText(), panel.getControl());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldInsertTabs(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      component.addTab("Tab1");
      Tab insertedTab = component.insertTab(0, "Tab2", panel);

      assertEquals(component.getTabs().get(0), insertedTab);

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).insertTab(0, insertedTab.getText(), panel.getControl());
    }

    @Test
    void shouldThrowDwcjRuntimeException() throws BBjException {
      doThrow(BBjException.class).when(control).addTab(anyString(), any(BBjControl.class));
      assertThrows(WebforjRuntimeException.class,
          () -> component.addTab("Tab1", new DwcComponentMock()));
    }

    @Test
    void shouldThrowsNullPointerException() {
      assertThrows(NullPointerException.class,
          () -> component.addTab((Tab) null, new DwcComponentMock()));
    }

    @Test
    void shouldThrowIllegalArgumentException() {
      Tab tab = new Tab("Tab1");
      component.addTab(tab);
      assertThrows(IllegalArgumentException.class,
          () -> component.addTab(tab, new DwcComponentMock()));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetTabNameToComponentName(boolean controlDefined)
        throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      component.setName("ComponentName");
      component.add(panel);

      assertTrue(component.hasComponent(panel));
      assertEquals(1, component.getComponents().size());
      assertEquals(1, component.getTabs().size());
      assertEquals(component.getName(), component.getTabs().get(0).getText());

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).addTab(component.getName(), panel.getControl());
    }

    @Test
    void addShouldThrowExceptionForDuplicateComponent() {
      DwcComponentMock panel = new DwcComponentMock();
      component.add(panel);
      assertThrows(IllegalArgumentException.class, () -> component.add(panel));
    }

    @Test
    void addShouldThrowExceptionWhenAddingDestroyedComponent() {
      DwcComponentMock destroyedComponent = spy(new DwcComponentMock());
      when(destroyedComponent.isDestroyed()).thenReturn(true);

      IllegalStateException exception =
          assertThrows(IllegalStateException.class, () -> component.add(destroyedComponent));
      assertTrue(exception.getMessage().contains("is destroyed"));
    }
  }

  @Nested
  @DisplayName("Set/Get ComponentFor API")
  class SetGetComponentForApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetComponentForGivenTab(boolean controlDefined)
        throws IllegalAccessException, BBjException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      Tab tab = component.addTab("Tab1");

      assertNull(component.getComponentFor(tab));

      component.setComponentFor(tab, panel);

      assertTrue(component.hasComponent(panel));
      assertEquals(1, component.getComponents().size());
      assertEquals(1, component.getTabs().size());
      assertEquals(tab, component.getTabs().get(0));
      assertEquals(panel, component.getComponentFor(tab));

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).setControlAt(0, panel.getControl());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetComponentForGivenTabIndex(boolean controlDefined)
        throws IllegalAccessException, BBjException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      component.addTab("Tab1");

      assertNull(component.getComponentFor(0));

      component.setComponentFor(0, panel);

      assertTrue(component.hasComponent(panel));
      assertEquals(1, component.getComponents().size());
      assertEquals(1, component.getTabs().size());
      assertEquals(panel, component.getComponentFor(0));

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).setControlAt(0, panel.getControl());
    }

    @Test
    void shouldDestroyOldComponent() {
      DwcComponentMock oldPanel = new DwcComponentMock();
      DwcComponentMock newPanel = new DwcComponentMock();
      component.addTab("Tab1", oldPanel);

      component.setComponentFor(0, newPanel);

      assertTrue(oldPanel.isDestroyed());
      assertFalse(component.hasComponent(oldPanel));
      assertTrue(component.hasComponent(newPanel));

      assertEquals(1, component.getComponents().size());
      assertEquals(1, component.getTabs().size());
      assertEquals(newPanel, component.getComponentFor(0));
    }

    @Test
    void shouldThrowDwcjRuntimeException() throws BBjException {
      component.addTab("Tab1", new DwcComponentMock());

      doThrow(BBjException.class).when(control).setControlAt(anyInt(), any(BBjControl.class));
      assertThrows(WebforjRuntimeException.class,
          () -> component.setComponentFor(0, new DwcComponentMock()));
    }
  }

  @Nested
  @DisplayName("Removing tabs API")
  class RemoveTabsAndComponentsApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldRemoveTab(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      Tab tab = component.addTab("Tab1", panel);

      component.removeTab(tab);

      assertFalse(component.hasComponent(panel));
      assertFalse(component.hasTab(tab));

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).removeTab(0);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldRemoveTabByIndex(boolean controlDefined)
        throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      Tab tab = component.addTab("Tab1", panel);

      component.removeTab(0);

      assertFalse(component.hasComponent(panel));
      assertFalse(component.hasTab(tab));

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).removeTab(0);
    }

    @Test
    void removingTabWillDestroyComponent() {
      DwcComponentMock panel = new DwcComponentMock();
      component.addTab("Tab1", panel);

      component.removeTab(0);

      assertTrue(panel.isDestroyed());
    }

    @Test
    void shouldThrowDwcjRuntimeException() throws BBjException {
      component.addTab("Tab1", new DwcComponentMock());

      doThrow(BBjException.class).when(control).removeTab(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.removeTab(0));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldRemoveAllTabs(boolean controlDefined) throws BBjException, IllegalAccessException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel1 = new DwcComponentMock();
      DwcComponentMock panel2 = new DwcComponentMock();
      component.addTab("Tab1", panel1);
      component.addTab("Tab2", panel2);

      component.removeAllTabs();

      assertFalse(component.hasComponent(panel1));
      assertFalse(component.hasComponent(panel2));
      assertTrue(component.getComponents().isEmpty());
      assertTrue(component.isEmpty());

      int times = controlDefined ? 2 : 0;
      verify(control, times(times)).removeTab(anyInt());
    }

    @Test
    void shouldRemoveComponent() {
      DwcComponentMock panel = new DwcComponentMock();
      component.addTab("Tab1", panel);

      component.remove(panel);

      assertFalse(component.hasComponent(panel));
      assertTrue(component.getComponents().isEmpty());
      assertFalse(component.isEmpty());
      assertNull(component.getComponentFor(0));
    }

    @Test
    void shouldRemoveAllComponents() {
      DwcComponentMock panel1 = new DwcComponentMock();
      DwcComponentMock panel2 = new DwcComponentMock();
      component.addTab("Tab1", panel1);
      component.addTab("Tab2", panel2);

      component.removeAll();

      assertFalse(component.hasComponent(panel1));
      assertFalse(component.hasComponent(panel2));
      assertTrue(component.getComponents().isEmpty());
      assertFalse(component.isEmpty());

      assertNull(component.getComponentFor(0));
      assertNull(component.getComponentFor(1));
    }
  }

  @Nested
  @DisplayName("Selecting tabs API")
  class SelectingTabsApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSelectTab(boolean controlDefined) throws IllegalAccessException, BBjException {
      if (!controlDefined) {
        ReflectionUtils.nullifyControl(component);
      }

      DwcComponentMock panel = new DwcComponentMock();
      Tab addedTab = component.addTab("Tab1", panel);

      component.select(0);

      assertEquals(0, component.getSelectedIndex());
      assertEquals(addedTab, component.getSelected());

      int times = controlDefined ? 1 : 0;
      verify(control, times(times)).setSelectedIndex(0);
    }

    @Test
    void selectedShouldThrowDwcjRuntimeException() throws BBjException {
      component.addTab("Tab1", new DwcComponentMock());

      doThrow(BBjException.class).when(control).setSelectedIndex(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.select(0));
    }

    @Test
    void getSelectedShouldThrowDwcjRuntimeException() throws BBjException {
      component.addTab("Tab1", new DwcComponentMock());

      doThrow(BBjException.class).when(control).getSelectedIndex();
      assertThrows(WebforjRuntimeException.class, () -> component.getSelected());
    }

    @Test
    void onAttachSelectionShouldBeApplied() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.addTab("Tab1", new DwcComponentMock());
      component.addTab("Tab2", new DwcComponentMock());

      component.select(1);

      verify(control, times(0)).setSelectedIndex(0);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setSelectedIndex(1);
    }
  }

  @Test
  void shouldGetTabForComponent() {
    DwcComponentMock panel = new DwcComponentMock();
    component.addTab("Tab1", panel);

    assertEquals(component.getTabs().get(0), component.getTabFor(panel));
  }

  @ParameterizedTest
  @EnumSource(TabbedPane.Placement.class)
  void shouldSetPlacement(TabbedPane.Placement placement) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setPlacement(placement);
    assertEquals(component.getPlacement(), placement);
    assertEquals(component.getProperty("placement"), placement.name().toLowerCase());
  }

  @ParameterizedTest
  @EnumSource(TabbedPane.Activation.class)
  void shouldSetActivation(TabbedPane.Activation activation) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setActivation(activation);
    assertEquals(component.getActivation(), activation);
    assertEquals(component.getProperty("activation"), activation.name().toLowerCase());
  }

  @ParameterizedTest
  @EnumSource(TabbedPane.Removal.class)
  void shouldSetRemoval(TabbedPane.Removal removal) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setRemoval(removal);
    assertEquals(component.getRemoval(), removal);
    assertEquals(component.getProperty("removal"), removal.name().toLowerCase());
  }

  @ParameterizedTest
  @EnumSource(TabbedPane.Alignment.class)
  void shouldSetAlignment(TabbedPane.Alignment alignment) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setAlignment(alignment);
    assertEquals(component.getAlignment(), alignment);
    assertEquals(component.getProperty("alignment"), alignment.name().toLowerCase());
  }

  @Test
  void shouldSetBorderless() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setBorderless(true);
    assertTrue(component.isBorderless());
    assertEquals(true, component.getProperty("borderless"));
  }

  @Test
  void shouldSetHideActiveIndicator() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setHideActiveIndicator(true);
    assertTrue(component.isHideActiveIndicator());
    assertEquals(true, component.getProperty("hideActiveIndicator"));
  }

  @Test
  void shouldSetNobody() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.hideBody(true);
    assertTrue(component.isBodyHidden());
    assertEquals(true, component.getProperty("nobody"));
  }

  @Test
  void shouldSetLabel() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setLabel("MyLabel");
    assertEquals("MyLabel", component.getLabel());
    assertEquals("MyLabel", component.getProperty("label"));
  }

  @Test
  void shouldSetSwipeable() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setSwipeable(true);
    assertTrue(component.isSwipeable());
    assertEquals(true, component.getProperty("swipeable"));
  }

  @Test
  void shouldSetSwipeWithMouseWheel() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setSwipeWithMouse(true);
    assertTrue(component.isSwipeWithMouse());
    assertEquals(true, component.getProperty("swipeWithMouse"));
  }

  @Test
  @DisplayName("adding/removing supported events")
  void addingRemovingSupportedEvents() {
    // TabSelectEvent, TabDeselectEvent, TabCloseEvent
    EventListener<TabSelectEvent> selectListener = event -> {
    };
    EventListener<TabDeselectEvent> deselectListener = event -> {
    };
    EventListener<TabCloseEvent> closeListener = event -> {
    };


    ListenerRegistration<TabSelectEvent> r1 = component.onSelect(selectListener);
    ListenerRegistration<TabDeselectEvent> r2 = component.onDeselect(deselectListener);
    ListenerRegistration<TabCloseEvent> r3 = component.onClose(closeListener);

    assertEquals(1, component.getEventListeners(TabSelectEvent.class).size());
    assertEquals(1, component.getEventListeners(TabDeselectEvent.class).size());
    assertEquals(1, component.getEventListeners(TabCloseEvent.class).size());

    r1.remove();
    r2.remove();
    r3.remove();

    assertEquals(0, component.getEventListeners(TabSelectEvent.class).size());
    assertEquals(0, component.getEventListeners(TabDeselectEvent.class).size());
    assertEquals(0, component.getEventListeners(TabCloseEvent.class).size());
  }
}
