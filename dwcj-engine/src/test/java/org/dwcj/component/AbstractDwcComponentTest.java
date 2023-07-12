package org.dwcj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.exceptions.DwcjRestrictedAccessException;
import org.dwcj.exceptions.DwcjRuntimeException;
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
public class AbstractDwcComponentTest {
  @Mock
  BBjEditBox control;

  @InjectMocks
  AbstractDwcComponentMock component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  void unnullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", control, true);
  }

  void invokeCatchUp(AbstractDwcComponentMock component)
      throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
    MethodUtils.invokeMethod(component, true, "catchUp");
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

      verify(control, times(1)).putClientProperty("expanse", expanse.getValue());
      verify(control, times(0)).getClientProperty("expanse");
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

      verify(control, times(1)).putClientProperty("theme", theme.getValue());
      verify(control, times(0)).getClientProperty("theme");
    }
  }

  @Nested
  @DisplayName("ReadOnly API")
  class ReadOnlyApi {

    @Test
    @DisplayName("Setting/getting readonly when control is null")
    void settingGettingReadOnlyWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
      component.setReadOnly(true);
      assertTrue(component.isReadOnly());

      verify(control, times(0)).setEditable(true);
      verify(control, times(0)).isEditable();
    }

    @Test
    @DisplayName("Setting/getting readonly when control is not null")
    void settingGettingReadOnlyWhenControlIsNotNull() throws BBjException {
      when(control.isEditable()).thenReturn(true);
      component.setReadOnly(true);

      assertTrue(component.isReadOnly());

      verify(control, times(1)).setEditable(true);
      verify(control, times(1)).isEditable();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setEditable(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setReadOnly(true));

      doThrow(BBjException.class).when(control).isEditable();
      assertThrows(DwcjRuntimeException.class, () -> component.isReadOnly());
    }

    @Test
    @DisplayName("catchup will re-apply readonly changes")
    void catchupWillReApplyingReadOnlyChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      component.setReadOnly(true);
      invokeCatchUp(component);
      verify(control, times(2)).setEditable(true);
    }
  }

  @Nested
  @DisplayName("Attributes API")
  class AttributesApi {

    @Test
    @DisplayName("Setting/getting attributes when control is null")
    void settingGettingAttributesWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
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

      assertThrows(DwcjRuntimeException.class, () -> component.setAttribute("key", "value"));
      assertThrows(DwcjRuntimeException.class, () -> component.getAttribute("key"));
    }

    @Test
    @DisplayName("""
        setAttribute will will throw DwcjRestrictedAccessException when
        attribute is restricted
        """)
    void setAttributeWillThrowDwcjRestrictedAccessExceptionWhenAttributeIsRestricted()
        throws IllegalAccessException, BBjException {
      nullifyControl();

      List<String> restrictedAttributes = component.getRestrictedAttributes();
      for (String restrictedAttribute : restrictedAttributes) {
        assertThrows(DwcjRestrictedAccessException.class,
            () -> component.setAttribute(restrictedAttribute, "value"));
      }
    }

    @Test
    @DisplayName("Remove attribute when control is null")
    void removeAttributeWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
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
      assertThrows(DwcjRuntimeException.class, () -> component.removeAttribute("key"));
    }

    @Test
    @DisplayName("catchup will re-apply attributes changes")
    void catchupWillReApplyingAttributesChanges() throws NoSuchMethodException,
        IllegalAccessException, InvocationTargetException, BBjException {
      nullifyControl();

      component.setAttribute("key-1", "value-1");
      component.setAttribute("key-2", "value-2");

      unnullifyControl();
      invokeCatchUp(component);

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
      nullifyControl();

      component.setProperty("key", "value");

      assertSame("value", component.getProperty("key"));

      verify(control, times(0)).putClientProperty("key", "value");
      verify(control, times(0)).getClientProperty("key");
    }

    @Test
    @DisplayName("Setting/getting properties when control is not null")
    void settingGettingPropertiesWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getClientProperty("key");

      component.setProperty("key", "value");
      assertSame("value", component.getProperty("key"));

      verify(control, times(1)).putClientProperty("key", "value");
      verify(control, times(1)).getClientProperty("key");
    }

    @Test
    @DisplayName("""
        Setting/getting properties when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void settingGettingPropertiesWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).putClientProperty("key", "value");
      doThrow(BBjException.class).when(control).getClientProperty("key");

      assertThrows(DwcjRuntimeException.class, () -> component.setProperty("key", "value"));
      assertThrows(DwcjRuntimeException.class, () -> component.getProperty("key"));
    }

    @Test
    @DisplayName("""
        setProperty will will throw DwcjRestrictedAccessException when
        property is restricted
        """)
    void setPropertyWillThrowDwcjRestrictedAccessExceptionWhenPropertyIsRestricted()
        throws IllegalAccessException, BBjException {
      nullifyControl();

      List<String> restrictedProperties = component.getRestrictedProperties();
      for (String restrictedProperty : restrictedProperties) {
        assertThrows(DwcjRestrictedAccessException.class,
            () -> component.setProperty(restrictedProperty, "value"));
      }
    }

    @Test
    @DisplayName("catchup will re-apply properties changes")
    void catchupWillReApplyingPropertiesChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      nullifyControl();
      component.setProperty("key-1", "value-1");
      component.setProperty("key-2", "value-2");

      unnullifyControl();
      invokeCatchUp(component);

      verify(control, times(1)).putClientProperty("key-1", "value-1");
      verify(control, times(1)).putClientProperty("key-2", "value-2");
    }
  }

  @Nested
  @DisplayName("Text API")
  class TextApi {

    @Test
    @DisplayName("Setting/getting text when control is null")
    void settingGettingTextWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();

      component.setText("value");
      assertSame("value", component.getText());

      verify(control, times(0)).setText("value");
      verify(control, times(0)).getText();
    }

    @Test
    @DisplayName("Setting/getting text when control is not null")
    void settingGettingTextWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getText();

      component.setText("value");
      assertSame("value", component.getText());

      verify(control, times(1)).setText("value");
      verify(control, times(1)).getText();
    }

    @Test
    @DisplayName("When value is null then empty string is returned")
    void whenValueIsNullThenEmptyStringIsReturned() throws IllegalAccessException {
      nullifyControl();

      component.setText(null);
      assertSame("", component.getText());
    }

    @Test
    @DisplayName("""
        Setting/getting text when control throws
        BBjException a DwcjRuntimeException is thrown
        """)
    void settingGettingTextWhenControlThrowsBbjException() throws BBjException {
      doThrow(BBjException.class).when(control).setText("value");
      doThrow(BBjException.class).when(control).getText();

      assertThrows(DwcjRuntimeException.class, () -> component.setText("value"));
      assertThrows(DwcjRuntimeException.class, () -> component.getText());
    }

    @Test
    @DisplayName("catchup will re-apply text changes")
    void catchupWillReApplyingTextChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      component.setText("value");

      invokeCatchUp(component);

      verify(control, times(2)).setText("value");
    }
  }

  @Nested
  @DisplayName("HighlightOnFocus API")
  class HighlightOnFocusApi {

    @Test
    @DisplayName("Setting/getting highlightOnFocus when control is null")
    void settingGettingHighlightOnFocusWhenControlIsNull()
        throws IllegalAccessException, BBjException {
      nullifyControl();

      component.setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL);
      assertSame(HighlightableOnFocus.Behavior.ALL, component.getHighlightOnFocus());

      verify(control, times(0)).setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL.getValue());
      verify(control, times(0)).getHighlightOnFocus();
    }

    @Test
    @DisplayName("Setting/getting highlightOnFocus when control is not null")
    void settingGettingHighlightOnFocusWhenControlIsNotNull() throws BBjException {
      component.setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL);
      assertSame(HighlightableOnFocus.Behavior.ALL, component.getHighlightOnFocus());

      verify(control, times(1)).setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL.getValue());
      verify(control, times(0)).getHighlightOnFocus();
    }

    @Test
    @DisplayName("catchup will re-apply highlightOnFocus changes")
    void catchupWillReApplyingHighlightOnFocusChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      component.setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL);
      invokeCatchUp(component);
      verify(control, times(2)).setHighlightOnFocus(HighlightableOnFocus.Behavior.ALL.getValue());
    }
  }

  @Nested
  @DisplayName("Alignment API")
  class AlignmentApi {

    @Test
    @DisplayName("set/get alignment when control is null")
    void setGetAlignment() throws BBjException, IllegalAccessException {
      nullifyControl();

      component.setHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT);
      assertEquals(AbstractDwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }

    @Test
    @DisplayName("set/get alignment when control is defined")
    void setGetAlignmentWhenControlIsDefined() throws BBjException {
      doReturn(AbstractDwcComponentMock.Alignment.RIGHT.getValue()).when(control).getAlignment();

      component.setHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT);
      assertEquals(AbstractDwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(1)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
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

      assertThrows(DwcjRuntimeException.class,
          () -> component.setHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT));
      assertThrows(DwcjRuntimeException.class, () -> component.getHorizontalAlignment());
    }

    @Test
    @DisplayName("catchup will reapply alignment changes")
    void catchupWillReapplyAlignmentChanges() throws BBjException, IllegalAccessException,
        NoSuchMethodException, InvocationTargetException {
      nullifyControl();

      component.setHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT);
      assertEquals(AbstractDwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();

      unnullifyControl();
      invokeCatchUp(component);

      verify(control, times(1)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }

    @Test
    @DisplayName("catchup will skip alignment changes if it is the default alignment")
    void catchupWillSkipAlignmentChangesIfItIsTheDefaultAlignment() throws BBjException,
        IllegalAccessException, NoSuchMethodException, InvocationTargetException {
      nullifyControl();

      component.setDefaultHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT);
      component.setHorizontalAlignment(AbstractDwcComponentMock.Alignment.RIGHT);

      assertEquals(AbstractDwcComponentMock.Alignment.RIGHT, component.getHorizontalAlignment());

      verify(control, times(0)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();

      unnullifyControl();
      invokeCatchUp(component);

      verify(control, times(0)).setAlignment(AbstractDwcComponentMock.Alignment.RIGHT.getValue());
      verify(control, times(0)).getAlignment();
    }
  }
}

