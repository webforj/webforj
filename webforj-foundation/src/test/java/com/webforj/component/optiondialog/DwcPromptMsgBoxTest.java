package com.webforj.component.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.optiondialog.DwcPromptMsgBox.MessageType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DwcPromptMsgBoxTest {

  private DwcPromptMsgBox<?> component;

  @BeforeEach
  void setUp() {
    component = new DwcPromptMsgBox<>();
  }

  @Test
  void shouldSetAndGetTitle() {
    component.setTitle("Test Title");
    assertEquals("Test Title", component.getTitle());
  }

  @Test
  void shouldSetAndGetMessage() {
    component.setMessage("Test Message");
    assertEquals("Test Message", component.getMessage());
  }

  @Test
  void shouldSetAndGetMessageType() {
    component.setMessageType(MessageType.QUESTION);
    assertEquals(MessageType.QUESTION, component.getMessageType());
  }

  @Test
  void shouldDetectButtonThemeFromDialogTheme() {
    assertEquals(ButtonTheme.DANGER,
        DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.DANGER));
    assertEquals(ButtonTheme.INFO, DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.INFO));
    assertEquals(ButtonTheme.PRIMARY,
        DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.PRIMARY));
    assertEquals(ButtonTheme.SUCCESS,
        DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.SUCCESS));
    assertEquals(ButtonTheme.WARNING,
        DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.WARNING));
    assertEquals(ButtonTheme.GRAY, DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.GRAY));
    assertEquals(ButtonTheme.DEFAULT,
        DwcPromptMsgBox.detectButtonThemeFromDialogTheme(Theme.DEFAULT));
  }

  @Test
  void shouldDetectDialogThemeFromMessageType() {
    assertEquals(Theme.DANGER, DwcPromptMsgBox.detectDialogThemeFromMessageType(MessageType.ERROR));
    assertEquals(Theme.INFO, DwcPromptMsgBox.detectDialogThemeFromMessageType(MessageType.INFO));
    assertEquals(Theme.WARNING,
        DwcPromptMsgBox.detectDialogThemeFromMessageType(MessageType.WARNING));
    assertEquals(Theme.PRIMARY,
        DwcPromptMsgBox.detectDialogThemeFromMessageType(MessageType.QUESTION));
    assertEquals(Theme.DEFAULT,
        DwcPromptMsgBox.detectDialogThemeFromMessageType(MessageType.PLAIN));
  }
}
