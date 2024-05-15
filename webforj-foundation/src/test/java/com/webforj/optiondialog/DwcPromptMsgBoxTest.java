package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;
import com.webforj.optiondialog.DwcPromptMsgBox.MessageType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DwcPromptMsgBoxTest {

  private DwcPromptMsgBox<?> dialog;

  @BeforeEach
  void setUp() {
    dialog = new DwcPromptMsgBox<>();
  }

  @Test
  void shouldSetAndGetTitle() {
    dialog.setTitle("Test Title");
    assertEquals("Test Title", dialog.getTitle());
  }

  @Test
  void shouldSetAndGetMessage() {
    dialog.setMessage("Test Message");
    assertEquals("Test Message", dialog.getMessage());
  }

  @Test
  void shouldSetAndGetMessageType() {
    dialog.setMessageType(MessageType.QUESTION);
    assertEquals(MessageType.QUESTION, dialog.getMessageType());
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
