package com.webforj.component.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.optiondialog.DwcPromptMsgBox.MessageType;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class MessageDialogTest {

  @Nested
  class Constructors {

    @Test
    void shouldCreateMessageDialogWithAllParameters() {
      MessageDialog dialog =
          new MessageDialog("Message", "Title", "Okay", MessageType.INFO, Theme.DANGER);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Okay", dialog.getButtonText());
      assertEquals(MessageType.INFO, dialog.getMessageType());
      assertEquals(Theme.DANGER, dialog.getTheme());
      assertEquals(ButtonTheme.DANGER, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithTitleMessageTypeAndTheme() {
      MessageDialog dialog = new MessageDialog("Message", "Title", MessageType.INFO, Theme.PRIMARY);


      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(MessageDialog.DEFAULT_BUTTON_TEXT, dialog.getButtonText());
      assertEquals(MessageType.INFO, dialog.getMessageType());
      assertEquals(Theme.PRIMARY, dialog.getTheme());
      assertEquals(ButtonTheme.PRIMARY, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithTitleMessageAndButtonType() {
      MessageDialog dialog = new MessageDialog("Message", "Title", "Okay", MessageType.WARNING);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Okay", dialog.getButtonText());
      assertEquals(MessageType.WARNING, dialog.getMessageType());
      assertEquals(Theme.WARNING, dialog.getTheme());
      assertEquals(ButtonTheme.WARNING, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithTitleAndButtonText() {
      MessageDialog dialog = new MessageDialog("Message", "Title", "Okay");

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Okay", dialog.getButtonText());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
      assertEquals(ButtonTheme.DEFAULT, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithTitleMessageAndMessageType() {
      MessageDialog dialog = new MessageDialog("Message", "Title", MessageType.QUESTION);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(MessageDialog.DEFAULT_BUTTON_TEXT, dialog.getButtonText());
      assertEquals(MessageType.QUESTION, dialog.getMessageType());
      assertEquals(Theme.PRIMARY, dialog.getTheme());
      assertEquals(ButtonTheme.PRIMARY, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithTitleAndMessage() {
      MessageDialog dialog = new MessageDialog("Message", "Title");

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(MessageDialog.DEFAULT_BUTTON_TEXT, dialog.getButtonText());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
      assertEquals(ButtonTheme.DEFAULT, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithMessage() {
      MessageDialog dialog = new MessageDialog("Message");


      assertEquals(MessageDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(MessageDialog.DEFAULT_BUTTON_TEXT, dialog.getButtonText());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
      assertEquals(ButtonTheme.DEFAULT, dialog.getButtonTheme());
    }

    @Test
    void shouldCreateMessageDialogWithNoParameters() {
      MessageDialog dialog = new MessageDialog();

      assertEquals(MessageDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("", dialog.getMessage());
      assertEquals(MessageDialog.DEFAULT_BUTTON_TEXT, dialog.getButtonText());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
      assertEquals(ButtonTheme.DEFAULT, dialog.getButtonTheme());
    }
  }

  @Test
  void shouldDetectButtonThemeFromDialogTheme() {
    assertEquals(ButtonTheme.DANGER, MessageDialog.detectButtonThemeFromDialogTheme(Theme.DANGER));
    assertEquals(ButtonTheme.INFO, MessageDialog.detectButtonThemeFromDialogTheme(Theme.INFO));
    assertEquals(ButtonTheme.PRIMARY,
        MessageDialog.detectButtonThemeFromDialogTheme(Theme.PRIMARY));
    assertEquals(ButtonTheme.SUCCESS,
        MessageDialog.detectButtonThemeFromDialogTheme(Theme.SUCCESS));
    assertEquals(ButtonTheme.WARNING,
        MessageDialog.detectButtonThemeFromDialogTheme(Theme.WARNING));
    assertEquals(ButtonTheme.GRAY, MessageDialog.detectButtonThemeFromDialogTheme(Theme.GRAY));
    assertEquals(ButtonTheme.DEFAULT,
        MessageDialog.detectButtonThemeFromDialogTheme(Theme.DEFAULT));
  }

  @Test
  void shouldDetectDialogThemeFromMessageType() {
    assertEquals(Theme.DANGER, MessageDialog.detectDialogThemeFromMessageType(MessageType.ERROR));
    assertEquals(Theme.INFO, MessageDialog.detectDialogThemeFromMessageType(MessageType.INFO));
    assertEquals(Theme.WARNING,
        MessageDialog.detectDialogThemeFromMessageType(MessageType.WARNING));
    assertEquals(Theme.PRIMARY,
        MessageDialog.detectDialogThemeFromMessageType(MessageType.QUESTION));
    assertEquals(Theme.DEFAULT, MessageDialog.detectDialogThemeFromMessageType(MessageType.PLAIN));
  }
}
