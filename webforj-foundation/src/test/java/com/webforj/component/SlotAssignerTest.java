
package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SlotAssignerTest {

  SlotAssigner slotAssigner;
  Component containerComponent;
  SlotRegistry slotRegistry;
  BBjControl controlMock;

  @BeforeEach
  void setUp() {
    containerComponent = mock(Component.class);
    slotAssigner = new SlotAssigner(containerComponent);
  }

  @Test
  void shouldReturnSlotRegistry() {
    assertNotNull(slotAssigner.getSlotRegistry());
  }

  @Test
  void shouldReturnCustomSlotRegistry() {
    SlotRegistry customSlotRegistry = new SlotRegistry();
    slotAssigner = new SlotAssigner(containerComponent, null, customSlotRegistry);
    assertEquals(customSlotRegistry, slotAssigner.getSlotRegistry());
  }

  @Test
  void shouldAssignSlots() throws BBjException {
    SlotAssigner spy = spy(slotAssigner);

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    spy.assign("main", slotComponentMock);
    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    spy.attach();

    verify(containerWindowMock).add(slotComponentMock);
    verify(containerControlMock).setSlot("main", slotControlMock);
  }

  @Test
  void shouldThrowExceptionForSlotsAlreadyAssigned() {
    SlotAssigner spy = spy(slotAssigner);

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    spy.assign("main", slotComponentMock);
    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    spy.attach();
    assertThrows(IllegalStateException.class, () -> spy.attach());
  }

  @Test
  void shouldUseCustomAssigner() {
    AtomicReference<String> slotNameRef = new AtomicReference<>();
    AtomicReference<BBjControl> targetControlRef = new AtomicReference<>();
    AtomicReference<BBjControl> slotControlRef = new AtomicReference<>();
    SlotAssigner spy =
        spy(new SlotAssigner(containerComponent, (slot, targetControl, slotControl) -> {
          slotNameRef.set(slot);
          targetControlRef.set(targetControl);
          slotControlRef.set(slotControl);
        }));

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    spy.assign("main", slotComponentMock);
    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    spy.attach();

    assertEquals("main", slotNameRef.get());
    assertEquals(containerControlMock, targetControlRef.get());
    assertEquals(slotControlMock, slotControlRef.get());
  }

  @Test
  void shouldNotQueueAssignment() throws BBjException {
    SlotAssigner spy = spy(slotAssigner);
    doReturn(true).when(spy).isAttached();

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    spy.assign("main", slotComponentMock);

    verify(containerWindowMock).add(slotComponentMock);
    verify(containerControlMock).setSlot("main", slotControlMock);
  }

  @Test
  void shouldThrowExceptionIfSlotComponentIsDestroyed() {
    SlotAssigner spy = spy(slotAssigner);

    // Slot component
    Component slotComponentMock = mock(Component.class);
    doReturn(true).when(slotComponentMock).isDestroyed();

    assertThrows(IllegalStateException.class, () -> spy.assign("main", slotComponentMock));
  }

  @Test
  void shouldThrowExceptionIfSlotComponentIsDestroyedBeforeAttach() {
    SlotAssigner spy = spy(slotAssigner);

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    spy.assign("main", slotComponentMock);
    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    doReturn(true).when(slotComponentMock).isDestroyed();

    assertThrows(IllegalStateException.class, () -> spy.attach());
  }

  @Test
  void shouldCatchDefaultAssignerExceptions() throws BBjException {
    SlotAssigner spy = spy(slotAssigner);

    // Slot component
    Component slotComponentMock = mock(Component.class);
    BBjControl slotControlMock = mock(BBjControl.class);

    spy.assign("main", slotComponentMock);
    doReturn(slotControlMock).when(spy).getControl(slotComponentMock);

    // Container component
    Window containerWindowMock = mock(Window.class);
    BBjControl containerControlMock = mock(BBjControl.class);

    doReturn(containerWindowMock).when(containerComponent).getWindow();
    doReturn(containerControlMock).when(spy).getControl(containerComponent);

    doThrow(BBjException.class).when(containerControlMock).setSlot("main", slotControlMock);

    assertThrows(WebforjRuntimeException.class, () -> spy.attach());
  }
}
