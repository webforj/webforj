package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.event.MouseEnterEvent;
import com.webforj.component.event.MouseExitEvent;
import com.webforj.component.event.RightMouseDownEvent;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRestrictedAccessException;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcComponentTest {
  @Mock
  BBjEditBox control;

  @InjectMocks
  DwcComponentMock component;

  @Test
  @DisplayName("placeholder")
  void placeholder() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setComponentPlaceholder("placeholder");
    assertEquals("placeholder", component.getComponentPlaceholder());

    assertEquals("placeholder", component.getProperty("placeholder"));
  }

  @Nested
  @DisplayName("ClassNames API")
  class ClassNamesApi {

    @Test
    @DisplayName("Setting classes")
    void settingClasses() throws BBjException {
      component.addClassName("class1", "class2");

      verify(control, times(1)).addClass("class1");
      verify(control, times(1)).addClass("class2");
    }

    @Test
    @DisplayName("Removing classes")
    void removingClasses() throws BBjException {
      component.addClassName("class1", "class2");
      component.removeClassName("class1", "class2");

      verify(control, times(1)).removeClass("class1");
      verify(control, times(1)).removeClass("class2");
    }

    @Test
    @DisplayName("Catch BBjException and throw DwcjRuntimeException")
    void catchBbjExceptionAndThrowDwcjRuntimeException() throws BBjException {
      doThrow(BBjException.class).when(control).addClass(anyString());
      assertThrows(WebforjRuntimeException.class, () -> component.addClassName("class1"));

      doThrow(BBjException.class).when(control).removeClass(anyString());
      assertThrows(WebforjRuntimeException.class, () -> component.removeClassName("class1"));
    }
  }

  @Nested
  @DisplayName("Expanse API")
  class ExpanseApi {

    @ParameterizedTest
    @EnumSource(Expanse.class)
    @DisplayName("Setting/getting expanse")
    void settingGettingExpanse(Expanse expanse) throws BBjException {
      component.setExpanse(expanse);
      assertSame(component.getExpanse(), expanse);

      verify(control, times(1)).setProperty("expanse", Expanse.NONE.equals(expanse) ? "" : expanse);
      verify(control, times(0)).getProperty("expanse");
    }

    @DisplayName("Setting expanse to null, should set the expanse attribute to empty string")
    @Test
    void settingExpanseToNullShouldSetTheExpanseAttributeToEmptyString() throws BBjException {
      component.setExpanse(null);
      assertEquals(Expanse.NONE, component.getExpanse());

      verify(control, times(1)).setProperty("expanse", "");
      verify(control, times(0)).getProperty("expanse");
    }
  }

  @Nested
  @DisplayName("Theme API")
  class ThemeApi {

    @ParameterizedTest
    @EnumSource(Theme.class)
    @DisplayName("Setting/getting theme")
    void settingGettingExpanse(Theme theme) throws BBjException {
      component.setTheme(theme);
      assertSame(component.getTheme(), theme);

      verify(control, times(1)).setProperty("theme", theme);
      verify(control, times(0)).getProperty("theme");
    }

    @DisplayName("Setting theme to null, should set the theme attribute to null")
    @Test
    void settingThemeToNullShouldSetTheThemeAttributeToNull() throws BBjException {
      component.setTheme(null);
      assertNull(component.getTheme());

      verify(control, times(1)).setProperty("theme", "");
      verify(control, times(0)).getProperty("theme");
    }
  }

  @Nested
  @DisplayName("ReadOnly API")
  class ReadOnlyApi {

    @Test
    @DisplayName("Setting/getting readonly when control is null")
    void settingGettingReadOnlyWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setReadOnly(true);
      assertTrue(component.isReadOnly());

      verify(control, times(0)).setEditable(false);
      verify(control, times(0)).isEditable();
    }

    @Test
    @DisplayName("Setting/getting readonly when control is not null")
    void settingGettingReadOnlyWhenControlIsNotNull() throws BBjException {
      when(control.isEditable()).thenReturn(false);
      component.setReadOnly(true);

      assertTrue(component.isReadOnly());

      verify(control, times(1)).setEditable(false);
      verify(control, times(1)).isEditable();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setEditable(anyBoolean());
      assertThrows(WebforjRuntimeException.class, () -> component.setReadOnly(true));

      doThrow(BBjException.class).when(control).isEditable();
      assertThrows(WebforjRuntimeException.class, () -> component.isReadOnly());
    }

    @Test
    @DisplayName("onAttach will re-apply readonly changes")
    void onAttachWillReApplyingReadOnlyChanges() throws BBjException, IllegalAccessException {
      component.setReadOnly(true);
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();
      verify(control, times(2)).setEditable(false);
    }
  }

  @Nested
  @DisplayName("Attributes API")
  class AttributesApi {

    @Test
    @DisplayName("Setting/getting attributes when control is null")
    void settingGettingAttributesWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      verify(control, times(0)).setAttribute("key", "value");
      verify(control, times(0)).getAttribute("key");
    }

    @Test
    @DisplayName("Setting/getting attributes when control is not null")
    void settingGettingAttributesWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getAttribute("key");

      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      verify(control, times(1)).setAttribute("key", "value");
      verify(control, times(1)).getAttribute("key");
    }

    @Test
    @DisplayName("""
        Setting/getting attributes when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void settingGettingAttributesWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setAttribute("key", "value");
      doThrow(BBjException.class).when(control).getAttribute("key");

      assertThrows(WebforjRuntimeException.class, () -> component.setAttribute("key", "value"));
      assertThrows(WebforjRuntimeException.class, () -> component.getAttribute("key"));
    }

    @Test
    @DisplayName("""
        setAttribute will will throw DwcjRestrictedAccessException when
        attribute is restricted
        """)
    void setAttributeWillThrowDwcjRestrictedAccessExceptionWhenAttributeIsRestricted()
        throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      List<String> restrictedAttributes = component.getRestrictedAttributes();
      for (String restrictedAttribute : restrictedAttributes) {
        assertThrows(WebforjRestrictedAccessException.class,
            () -> component.setAttribute(restrictedAttribute, "value"));
      }
    }

    @Test
    @DisplayName("Remove attribute when control is null")
    void removeAttributeWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);
      component.setAttribute("key", "value");

      assertSame("value", component.getAttribute("key"));

      component.removeAttribute("key");
      assertSame(null, component.getAttribute("key"));

      verify(control, times(0)).removeAttribute("key");
    }

    @Test
    @DisplayName("Remove attribute when control is not null")
    void removeAttributeWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getAttribute("key");

      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      component.removeAttribute("key");
      verify(control, times(1)).removeAttribute("key");
    }

    @Test
    @DisplayName("""
        Remove attribute when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void removeAttributeWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).removeAttribute("key");
      assertThrows(WebforjRuntimeException.class, () -> component.removeAttribute("key"));
    }

    @Test
    @DisplayName("onAttach will re-apply attributes changes")
    void onAttachWillReApplyingAttributesChanges() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setAttribute("key-1", "value-1");
      component.setAttribute("key-2", "value-2");

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setAttribute("key-1", "value-1");
      verify(control, times(1)).setAttribute("key-2", "value-2");
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    @DisplayName("Setting/getting properties when control is null")
    void settingGettingPropertiesWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setProperty("key", "value");

      assertEquals("value", component.getProperty("key"));

      verify(control, times(0)).setProperty("key", "value");
      verify(control, times(0)).getProperty("key");
    }

    @Test
    @DisplayName("Getting properties will respect the passed type")
    void gettingPropertiesWillRespectThePassedType() throws BBjException {
      doReturn("value").when(control).getProperty("key", String.class);

      component.setProperty("key", "value");
      assertEquals("value", component.getProperty("key", String.class));

      verify(control, times(1)).setProperty("key", "value");
      verify(control, times(1)).getProperty("key", String.class);
    }

    @Test
    @DisplayName("""
        Setting/getting properties when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void settingGettingPropertiesWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setProperty("key", "value");
      doThrow(BBjException.class).when(control).getProperty("key", Object.class);

      assertThrows(WebforjRuntimeException.class, () -> component.setProperty("key", "value"));
      assertThrows(WebforjRuntimeException.class, () -> component.getProperty("key"));
    }

    @Test
    @DisplayName("onAttach will re-apply properties changes")
    void onAttachWillReApplyingPropertiesChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setProperty("key-1", "value-1");
      component.setProperty("key-2", "value-2");

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setProperty("key-1", "value-1");
      verify(control, times(1)).setProperty("key-2", "value-2");
    }
  }

  @Nested
  @DisplayName("Text API")
  class TextApi {

    @Test
    @DisplayName("Setting/getting text when control is null")
    void settingGettingTextWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setText("value");
      assertEquals("value", component.getText());

      verify(control, times(0)).setText("value");
      verify(control, times(0)).getText();
    }

    @Test
    @DisplayName("Setting/getting text when control is not null")
    void settingGettingTextWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getText();

      component.setText("value");
      assertEquals("value", component.getText());

      verify(control, times(1)).setText("value");
      verify(control, times(1)).getText();
    }

    @Test
    @DisplayName("When value is null then empty string is returned")
    void whenValueIsNullThenEmptyStringIsReturned() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setText(null);
      assertSame("", component.getText());
    }

    @Test
    @DisplayName("""
        Setting/getting text when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void settingGettingTextWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setText(anyString());
      doThrow(BBjException.class).when(control).getText();

      assertThrows(WebforjRuntimeException.class, () -> component.setText("value"));
      assertThrows(WebforjRuntimeException.class, () -> component.getText());
    }

    @Test
    @DisplayName("catchup will re-apply text changes")
    void catchupWillReApplyingTextChanges() throws BBjException, IllegalAccessException {
      component.setText("value");

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(2)).setText("value");
    }

    @Test
    void shouldSetGetHtmlWhenControlNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      String html = "<p>hello</p>";
      String expected = "<html><p>hello</p></html>";

      component.setHtml(html);
      assertEquals(html, component.getHtml());

      verify(control, times(0)).setText(expected);
      verify(control, times(0)).getText();
    }

    @Test
    void shouldSetGetHtmlWhenControlNotNull() throws BBjException {
      String html = "<p>hello</p>";
      String expected = "<html><p>hello</p></html>";

      doReturn(expected).when(control).getText();

      component.setHtml(html);
      assertEquals(html, component.getHtml());

      verify(control, times(1)).setText(expected);
      verify(control, times(1)).getText();
    }
  }

  @Nested
  @DisplayName("HighlightOnFocus API")
  class HighlightOnFocusApi {

    @Test
    @DisplayName("Setting/getting highlightOnFocus when control is null")
    void settingGettingHighlightOnFocusWhenControlIsNull()
        throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL);
      assertSame(HasHighlightOnFocus.Behavior.ALL, component.getHighlightOnFocus());

      verify(control, times(0)).setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL.getValue());
      verify(control, times(0)).getHighlightOnFocus();
    }

    @Test
    @DisplayName("Setting/getting highlightOnFocus when control is not null")
    void settingGettingHighlightOnFocusWhenControlIsNotNull() throws BBjException {
      component.setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL);
      assertSame(HasHighlightOnFocus.Behavior.ALL, component.getHighlightOnFocus());

      verify(control, times(1)).setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL.getValue());
      verify(control, times(0)).getHighlightOnFocus();
    }

    @Test
    @DisplayName("onAttach will re-apply highlightOnFocus changes")
    void onAttachWillReApplyingHighlightOnFocusChanges()
        throws BBjException, IllegalAccessException {
      component.setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL);
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();
      verify(control, times(2)).setHighlightOnFocus(HasHighlightOnFocus.Behavior.ALL.getValue());
    }
  }

  @Nested
  @DisplayName("Alignment API")
  class AlignmentApi {

    @Test
    @DisplayName("set/get alignment when control is null")
    void setGetAlignment() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setHorizontalAlignment(DwcComponentMock.Alignment.RIGHT);
      assertEquals(DwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }

    @Test
    @DisplayName("set/get alignment when control is defined")
    void setGetAlignmentWhenControlIsDefined() throws BBjException {
      doReturn(DwcComponentMock.Alignment.RIGHT.getValue()).when(control).getAlignment();

      component.setHorizontalAlignment(DwcComponentMock.Alignment.RIGHT);
      assertEquals(DwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(1)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(1)).getAlignment();
    }

    @Test
    @DisplayName("""
        set/get alignment will re-throw a DwcjRuntimeException when a
        BBjException is thrown by the control
        """)
    void settingAlignmentWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setAlignment(anyInt());
      doThrow(BBjException.class).when(control).getAlignment();

      assertThrows(WebforjRuntimeException.class,
          () -> component.setHorizontalAlignment(DwcComponentMock.Alignment.RIGHT));
      assertThrows(WebforjRuntimeException.class, () -> component.getHorizontalAlignment());
    }

    @Test
    @DisplayName("catchup will reapply alignment changes")
    void catchupWillReapplyAlignmentChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setHorizontalAlignment(DwcComponentMock.Alignment.RIGHT);
      assertEquals(DwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }

    @Test
    @DisplayName("catchup will skip alignment changes if it is the default alignment")
    void catchupWillSkipAlignmentChangesIfItIsTheDefaultAlignment()
        throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setDefaultHorizontalAlignment(DwcComponentMock.Alignment.RIGHT);
      component.setHorizontalAlignment(DwcComponentMock.Alignment.RIGHT);

      assertEquals(DwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(0)).setAlignment(DwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }
  }

  @Test
  @DisplayName("Test mouse events")
  void testMouseEvents() {
    EventListener<MouseEnterEvent> mouseEnterListener = event -> {
    };
    EventListener<MouseExitEvent> mouseExitListener = event -> {
    };
    EventListener<RightMouseDownEvent> rightMouseDownListener = event -> {
    };

    ListenerRegistration<MouseEnterEvent> r1 = component.onMouseEnter(mouseEnterListener);
    ListenerRegistration<MouseExitEvent> r2 = component.onMouseExit(mouseExitListener);
    ListenerRegistration<RightMouseDownEvent> r3 =
        component.onRightMouseDown(rightMouseDownListener);

    assertEquals(1, component.getEventListeners(MouseEnterEvent.class).size());
    assertEquals(1, component.getEventListeners(MouseExitEvent.class).size());
    assertEquals(1, component.getEventListeners(RightMouseDownEvent.class).size());

    r1.remove();
    r2.remove();
    r3.remove();

    assertEquals(0, component.getEventListeners(MouseEnterEvent.class).size());
    assertEquals(0, component.getEventListeners(MouseExitEvent.class).size());
    assertEquals(0, component.getEventListeners(RightMouseDownEvent.class).size());
  }

  @Nested
  class StyleApi {

    @Test
    void shouldSetGetStyleWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setStyle("key", "value");
      assertSame("value", component.getStyle("key"));

      verify(control, times(0)).setStyle("key", "value");
      verify(control, times(0)).getStyle("key");
    }

    @Test
    void shouldSetGetStyleWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getStyle("key");

      component.setStyle("key", "value");
      assertSame("value", component.getStyle("key"));

      verify(control, times(1)).setStyle("key", "value");
      verify(control, times(1)).getStyle("key");
    }

    @Test
    void shouldRemoveStyleWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setStyle("key", "value");
      assertSame("value", component.getStyle("key"));

      component.removeStyle("key");
      assertSame(null, component.getStyle("key"));

      verify(control, times(0)).unsetStyle("key");
    }

    @Test
    void shouldRemoveStyleWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getStyle("key");

      component.setStyle("key", "value");
      assertSame("value", component.getStyle("key"));

      component.removeStyle("key");
      verify(control, times(1)).unsetStyle("key");
    }

    @Test
    void shouldGetComputedStyleWhenControlNotNull() throws BBjException {
      doReturn("value").when(control).getComputedStyle("key");

      assertSame("value", component.getComputedStyle("key"));
      verify(control, times(1)).getComputedStyle("key");
    }

    @Test
    void shouldGetComputedStyleWhenControlNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setStyle("key", "value");
      assertSame("value", component.getComputedStyle("key"));
    }

    @Test
    void shouldCatchBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setStyle("key", "value");
      doThrow(BBjException.class).when(control).getStyle("key");
      doThrow(BBjException.class).when(control).unsetStyle("key");
      doThrow(BBjException.class).when(control).getComputedStyle("key");

      assertThrows(WebforjRuntimeException.class, () -> component.setStyle("key", "value"));
      assertThrows(WebforjRuntimeException.class, () -> component.getStyle("key"));
      assertThrows(WebforjRuntimeException.class, () -> component.removeStyle("key"));
      assertThrows(WebforjRuntimeException.class, () -> component.getComputedStyle("key"));
    }

    @Test
    void shouldReApplyingStyleChanges() throws BBjException, IllegalAccessException {
      component.setStyle("key1", "value1");
      component.setStyle("key2", "value2");
      component.removeStyle("key2");

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(2)).setStyle("key1", "value1");
    }
  }

  @Nested
  class SizeApi {

    @Test
    void shouldSetGetSize() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      DwcComponentMock spy = spy(component);
      spy.setSize("100px", "200px");
      spy.setMinSize("50px", "100px");
      spy.setMaxSize("200px", "300px");

      assertEquals("100px", spy.getWidth());
      assertEquals("50px", spy.getMinWidth());
      assertEquals("200px", spy.getMaxWidth());
      assertEquals("200px", spy.getHeight());
      assertEquals("100px", spy.getMinHeight());
      assertEquals("300px", spy.getMaxHeight());

      verify(spy, times(1)).setStyle("width", "100px");
      verify(spy, times(1)).setStyle("min-width", "50px");
      verify(spy, times(1)).setStyle("max-width", "200px");
      verify(spy, times(1)).setStyle("height", "200px");
      verify(spy, times(1)).setStyle("min-height", "100px");
      verify(spy, times(1)).setStyle("max-height", "300px");

      spy.setSize(null, null);
      spy.setMinSize(null, null);
      spy.setMaxSize(null, null);

      assertNull(spy.getWidth());
      assertNull(spy.getMinWidth());
      assertNull(spy.getMaxWidth());
      assertNull(spy.getHeight());
      assertNull(spy.getMinHeight());
      assertNull(spy.getMaxHeight());

      verify(spy, times(1)).removeStyle("width");
      verify(spy, times(1)).removeStyle("min-width");
      verify(spy, times(1)).removeStyle("max-width");
      verify(spy, times(1)).removeStyle("height");
      verify(spy, times(1)).removeStyle("min-height");
      verify(spy, times(1)).removeStyle("max-height");
    }

    @Test
    void shouldGetComputedSize() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      DwcComponentMock spy = spy(component);
      spy.setSize("100px", "200px");
      spy.setMinSize("50px", "100px");
      spy.setMaxSize("200px", "300px");

      assertEquals("100px", spy.getComputedWidth());
      assertEquals("50px", spy.getComputedMinWidth());
      assertEquals("200px", spy.getComputedMaxWidth());
      assertEquals("200px", spy.getComputedHeight());
      assertEquals("100px", spy.getComputedMinHeight());
      assertEquals("300px", spy.getComputedMaxHeight());
    }
  }

  @Nested
  class SlotsApi {

    @Test
    void shouldSetAndGetPrefix() {
      Component prefixComponent = mock(Component.class);
      component.setPrefixComponent(prefixComponent);
      assertEquals(prefixComponent, component.getPrefixComponent());
    }

    @Test
    void shouldDestroyExistingPrefixWhenSettingNull() {
      Component prefixComponent = mock(Component.class);
      component.setPrefixComponent(prefixComponent);
      assertEquals(prefixComponent, component.getPrefixComponent());

      component.setPrefixComponent(null);
      verify(prefixComponent, times(1)).destroy();
      assertNull(component.getPrefixComponent());
    }

    @Test
    void shouldSetAndGetSuffix() {
      Component suffixComponent = mock(Component.class);
      component.setSuffixComponent(suffixComponent);
      assertEquals(suffixComponent, component.getSuffixComponent());
    }

    @Test
    void shouldDestroyExistingSuffixWhenSettingNull() {
      Component suffixComponent = mock(Component.class);
      component.setSuffixComponent(suffixComponent);
      assertEquals(suffixComponent, component.getSuffixComponent());

      component.setSuffixComponent(null);
      verify(suffixComponent, times(1)).destroy();
      assertNull(component.getSuffixComponent());
    }

    @Test
    void shouldAttachPrefixAndSuffix() {
      component = spy(component);
      SlotAssigner slotAssigner = mock(SlotAssigner.class);
      doReturn(slotAssigner).when(component).getSlotAssigner();

      Component prefixComponent = mock(Component.class);
      Component suffixComponent = mock(Component.class);
      component.setPrefixComponent(prefixComponent);
      component.setSuffixComponent(suffixComponent);

      component.onAttach();

      verify(slotAssigner, times(1)).attach();
      verify(slotAssigner, times(1)).attach();
    }
  }

  @Nested
  class NameApi {

    @Test
    void shouldSetGetNameWhenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setName("name");
      assertEquals("name", component.getName());
    }

    @Test
    void shouldSetGetNameWhenControlIsNotNull() throws BBjException {
      component.setName("name");
      assertEquals("name", component.getName());

      verify(control, times(1)).setName("name");
    }

    @Test
    void shouldReApplyingNameChanges() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setName("name");
      assertEquals("name", component.getName());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setName("name");
    }
  }
}

