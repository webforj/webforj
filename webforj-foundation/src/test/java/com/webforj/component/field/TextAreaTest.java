package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.field.TextArea.WrapStyle;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.selection.SelectionRange;
import com.webforj.dispatcher.EventListener;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TextAreaTest {

  @Mock
  BBjCEdit control;

  @InjectMocks
  TextArea component = new TextArea();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new TextArea("label", "value", "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextArea("label", "value", listener);
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new TextArea("label", "value");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextArea("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextArea(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new TextArea("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new TextArea();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Nested
  class ParagraphsApi {

    @Test
    void shouldAddParagraphAtIndexWhenControlIsNotNull() throws BBjException {
      doNothing().when(control).addParagraph(anyInt(), anyString());

      component.addParagraph(0, "paragraph");
      verify(control).addParagraph(0, "paragraph");
    }

    @Test
    void shouldAddParagraphAtIndexWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.addParagraph(0, "paragraph 1");
      component.addParagraph(2, "paragraph 2");
      component.addParagraph(-5, "paragraph 3");

      assertEquals(3, component.getParagraphs().size());
      assertEquals("paragraph 1", component.getParagraphs().get(0));
      assertEquals("paragraph 2", component.getParagraphs().get(1));
      assertEquals("paragraph 3", component.getParagraphs().get(2));

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).addParagraph(-1, "paragraph 1");
      verify(control).addParagraph(-1, "paragraph 2");
    }

    @Test
    void shouldAddParagraphAtEnd() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.addParagraph("paragraph 1");
      component.addParagraph("paragraph 2");

      assertEquals(2, component.getParagraphs().size());
      assertEquals("paragraph 1", component.getParagraphs().get(0));
      assertEquals("paragraph 2", component.getParagraphs().get(1));
    }

    @Test
    void shouldAddListParagraphsAtIndex() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      List<String> paragraphs = new ArrayList<>();
      paragraphs.add("paragraph 1");
      paragraphs.add("paragraph 2");
      paragraphs.add("paragraph 3");

      component.addParagraphs(0, paragraphs);

      assertEquals(3, component.getParagraphs().size());
      assertEquals("paragraph 1", component.getParagraphs().get(0));
      assertEquals("paragraph 2", component.getParagraphs().get(1));
      assertEquals("paragraph 3", component.getParagraphs().get(2));
    }

    @Test
    void shouldAppendToParagraphAtIndexWhenControlIsNotNull() throws BBjException {
      doNothing().when(control).appendToParagraph(anyInt(), anyString());

      component.appendToParagraph(0, "text");
      verify(control).appendToParagraph(0, "text");
    }

    @Test
    void shouldAppendToParagraphAtIndexWhenControlIsNull() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.addParagraph(0, "text 1");
      component.appendToParagraph(0, " appended to text 1");
      component.appendToParagraph(1, "text 2");

      assertEquals(1, component.getParagraphs().size());
      assertEquals("text 1 appended to text 1", component.getParagraphs().get(0));
    }

    @Test
    void shouldGetAllParagraphsWhenControlIsNotNull() throws BBjException {
      when(control.getAllParagraphs())
          .thenReturn(new BBjVector(List.of("paragraph 1", "paragraph 2")));

      List<String> paragraphs = component.getParagraphs();

      assertEquals(2, paragraphs.size());
      assertEquals("paragraph 1", paragraphs.get(0));
      assertEquals("paragraph 2", paragraphs.get(1));
    }
  }

  @Nested
  class MaxLineCountLimitApi {

    @Test
    void shouldSetLineCountLimitWhenControlIsNotNull() throws BBjException {
      doNothing().when(control).setLineCountLimit(5);

      component.setLineCountLimit(5);
      assertEquals(5, component.getLineCountLimit());
      verify(control).setLineCountLimit(5);
    }

    @Test
    void shouldSetLineCountLimitWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setLineCountLimit(5);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setLineCountLimit(5);
    }

    @Test
    void shouldNotSetLineCountLimitWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setLineCountLimit(-5));
    }
  }

  @Nested
  class MaxParagraphLengthApi {

    @Test
    void shouldSetParagraphLengthLimitWhenControlIsNotNull() throws BBjException {
      doNothing().when(control).setMaxParagraphSize(5);

      component.setParagraphLengthLimit(5);
      assertEquals(5, component.getParagraphLengthLimit());
      verify(control).setMaxParagraphSize(5);
    }

    @Test
    void shouldSetParagraphLengthLimitWhenControlIsNull()
        throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setParagraphLengthLimit(5);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMaxParagraphSize(5);
    }

    @Test
    void shouldNotSetParagraphLengthLimitWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setParagraphLengthLimit(-5));
    }
  }

  @Nested
  class MaxLengthApi {

    @Test
    void shouldSetMaxLengthWhenControlIsNotNull() throws BBjException {
      doNothing().when(control).setMaxLength(5);

      component.setMaxLength(5);
      assertEquals(5, component.getMaxLength());
      verify(control).setMaxLength(5);
    }

    @Test
    void shouldSetMaxLengthWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setMaxLength(5);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMaxLength(5);
    }

    @Test
    void shouldNotSetMaxLengthWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setMaxLength(-5));
    }
  }

  @Nested
  class MinLengthApi {

    @Test
    void shouldSetMinLength() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setMinLength(5);

      assertEquals(5, component.getMinLength());
      assertEquals(5, component.getProperty("minLength", Integer.class));
    }

    @Test
    void shouldNotSetMinLengthWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setMinLength(-5));
    }
  }

  @Nested
  class RowsApi {
    @Test
    void shouldSetMinLength() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setRows(50);

      assertEquals(50, component.getRows());
      assertEquals(50, component.getProperty("rows", Integer.class));
    }

    @Test
    void shouldNotSetMinLengthWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setRows(-5));
    }
  }

  @Nested
  class ColumnsApi {
    @Test
    void shouldSetMinLength() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setColumns(50);

      assertEquals(50, component.getColumns());
      assertEquals(50, component.getProperty("cols", Integer.class));
    }

    @Test
    void shouldNotSetMinLengthWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setColumns(-5));
    }
  }

  @Nested
  class LineWrapApi {

    @Test
    void shouldSetLineWrapWhenControlIsNotNull() throws BBjException {
      component.setLineWrap(true);
      assertEquals(true, component.isLineWrap());
      verify(control).setLineWrap(true);
    }

    @Test
    void shouldSetLineWrapWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setLineWrap(true);
      assertEquals(true, component.isLineWrap());

      verify(control, times(0)).setLineWrap(true);
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setLineWrap(true);
    }
  }

  @Nested
  class HorizontalScrollApi {

    @Test
    void shouldSetHorizontalScrollWhenControlIsNotNull() throws BBjException {
      component.setHorizontalScroll(true);
      assertEquals(true, component.isHorizontalScroll());
      verify(control).setHorizontalScrollable(true);
    }

    @Test
    void shouldSetHorizontalScrollWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setHorizontalScroll(true);
      assertEquals(true, component.isHorizontalScroll());

      verify(control, times(0)).setHorizontalScrollable(true);
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setHorizontalScrollable(true);
    }
  }

  @Nested
  class VerticalScrollApi {

    @Test
    void shouldSetVerticalScrollWhenControlIsNotNull() throws BBjException {
      component.setVerticalScroll(true);
      assertEquals(true, component.isVerticalScroll());
      verify(control).setVerticalScrollable(true);
    }

    @Test
    void shouldSetVerticalScrollWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setVerticalScroll(true);
      assertEquals(true, component.isVerticalScroll());

      verify(control, times(0)).setVerticalScrollable(true);
      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setVerticalScrollable(true);
    }
  }

  @Nested
  class WrapStyleApi {

    @Test
    void shouldSetWrapStyleWhenControlIsNotNull() throws BBjException {
      component.setWrapStyle(WrapStyle.CHARACTER_BOUNDARIES);
      assertEquals(WrapStyle.CHARACTER_BOUNDARIES, component.getWrapStyle());

      verify(control).setWrapStyleWord(false);
    }

    @Test
    void shouldSetWrapStyleWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setWrapStyle(WrapStyle.CHARACTER_BOUNDARIES);
      assertEquals(WrapStyle.CHARACTER_BOUNDARIES, component.getWrapStyle());

      verify(control, times(0)).setWrapStyleWord(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setWrapStyleWord(false);
    }
  }

  @Nested
  class TypingModeApi {

    @Test
    void shouldSetTypingModeWhenControlIsNotNull() throws BBjException {
      component.setTypingMode(TextArea.TypingMode.OVERWRITE);
      assertEquals(TextArea.TypingMode.OVERWRITE, component.getTypingMode());
      verify(control).setOvertypeMode(true);
    }

    @Test
    void shouldSetTypingModeWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setTypingMode(TextArea.TypingMode.OVERWRITE);
      assertEquals(TextArea.TypingMode.OVERWRITE, component.getTypingMode());

      verify(control, times(0)).setOvertypeMode(true);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setOvertypeMode(true);
    }
  }

  @Nested
  class SelectionApi {

    @Test
    void shouldGetSelectedRangeWhenControlIsNotNull() throws BBjException {
      SelectionRange range = new SelectionRange(1, 0, 2, 0);
      when(control.getSelection()).thenReturn(new BBjVector(List.of(range.getStartParagraph(),
          range.getStartOffset(), range.getEndParagraph(), range.getEndOffset())));

      component.setSelectionRange(range);
      assertEquals(range, component.getSelectionRange());

      verify(control).highlight(range.getStartParagraph(), range.getStartOffset(),
          range.getEndParagraph(), range.getEndOffset());
    }

    @Test
    void shouldGetSelectedRangeWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      SelectionRange range = new SelectionRange(1, 0, 2, 0);
      component.setSelectionRange(range);
      assertEquals(range, component.getSelectionRange());

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).highlight(range.getStartParagraph(), range.getStartOffset(),
          range.getEndParagraph(), range.getEndOffset());
    }
  }

  @Nested
  class ColApi {
    @Test
    void shouldSetMinLength() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setColumns(50);

      assertEquals(50, component.getColumns());
      assertEquals(50, component.getProperty("cols", Integer.class));
    }

    @Test
    void shouldNotSetMinLengthWhenValueIsNegative() {
      assertThrows(IllegalArgumentException.class, () -> component.setColumns(-5));
    }
  }
}
