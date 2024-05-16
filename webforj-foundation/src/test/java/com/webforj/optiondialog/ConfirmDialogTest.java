package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.button.ButtonTheme;
import com.webforj.optiondialog.ConfirmDialog.Button;
import com.webforj.optiondialog.ConfirmDialog.OptionType;
import com.webforj.optiondialog.DwcPromptMsgBox.MessageType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ConfirmDialogTest {

  private ConfirmDialog component;

  @BeforeEach
  void setUp() {
    component = new ConfirmDialog();
  }

  @Nested
  class Constructors {
    @Test
    void shouldCreateMessageDialogWithAllParameters() {
      ConfirmDialog dialog =
          new ConfirmDialog("Message", "Title", OptionType.YES_NO, MessageType.INFO);
      assertNotNull(dialog);
      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(OptionType.YES_NO, dialog.getOptionType());
      assertEquals(MessageType.INFO, dialog.getMessageType());
    }

    @Test
    void shouldCreateMessageDialogWithMessageTypeOnly() {
      ConfirmDialog dialog = new ConfirmDialog("Message", "Title", OptionType.ABORT_RETRY_IGNORE);
      assertNotNull(dialog);
      assertEquals(OptionType.ABORT_RETRY_IGNORE, dialog.getOptionType());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
    }

    @Test
    void shouldCreateMessageDialogWithDefaults() {
      ConfirmDialog dialog = new ConfirmDialog("Message", "Title");
      assertNotNull(dialog);
      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(OptionType.OK, dialog.getOptionType());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
    }

    @Test
    void shouldCreateMessageDialogWithMessageOnly() {
      ConfirmDialog dialog = new ConfirmDialog("Message");
      assertNotNull(dialog);
      assertEquals("Message", dialog.getMessage());
      assertEquals(OptionType.OK, dialog.getOptionType());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
    }
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
  void shouldSetAndGetOptionTypeOfMessage() {
    component.setOptionType(OptionType.OK_CANCEL);
    assertEquals(OptionType.OK_CANCEL, component.getOptionType());
  }

  @Test
  void shouldSetAndGetMessageType() {
    component.setMessageType(MessageType.QUESTION);
    assertEquals(MessageType.QUESTION, component.getMessageType());
  }

  @Test
  void shouldSetAndGetDefaultButton() {
    component.setDefaultButton(Button.SECOND);
    assertEquals(Button.SECOND, component.getDefaultButton());
  }

  @Test
  void shouldEnableAndCheckRawText() {
    component.setRawText(true);
    assertTrue(component.isRawText());
  }

  @Test
  void shouldSetAndGetButtonsTexts() {
    component.setButtonText(Button.FIRST, "First");
    component.setButtonText(Button.SECOND, "Second");
    component.setButtonText(Button.THIRD, "Third");

    assertEquals("First", component.getFirstButtonText());
    assertEquals("Second", component.getSecondButtonText());
    assertEquals("Third", component.getThirdButtonText());
  }

  @Test
  void shouldSetAndGetButtonThemes() {
    component.setButtonTheme(Button.FIRST, ButtonTheme.DANGER);
    component.setButtonTheme(Button.SECOND, ButtonTheme.SUCCESS);
    component.setButtonTheme(Button.THIRD, ButtonTheme.WARNING);

    assertEquals("\"danger\"", component.getAttributes().get("button-0-theme"));
    assertEquals("\"success\"", component.getAttributes().get("button-1-theme"));
    assertEquals("\"warning\"", component.getAttributes().get("button-2-theme"));
  }

  @Test
  void shouldMapResult() {
    assertEquals(ConfirmDialog.Result.OK, component.mapResult(1));
    assertEquals(ConfirmDialog.Result.CANCEL, component.mapResult(2));
    assertEquals(ConfirmDialog.Result.ABORT, component.mapResult(3));
    assertEquals(ConfirmDialog.Result.RETRY, component.mapResult(4));
    assertEquals(ConfirmDialog.Result.IGNORE, component.mapResult(5));
    assertEquals(ConfirmDialog.Result.YES, component.mapResult(6));
    assertEquals(ConfirmDialog.Result.NO, component.mapResult(7));
    assertEquals(ConfirmDialog.Result.TIMEOUT, component.mapResult(-1));

    component.setOptionType(OptionType.CUSTOM);
    assertEquals(ConfirmDialog.Result.FIRST_CUSTOM_BUTTON, component.mapResult(1));
    assertEquals(ConfirmDialog.Result.SECOND_CUSTOM_BUTTON, component.mapResult(2));
    assertEquals(ConfirmDialog.Result.THIRD_CUSTOM_BUTTON, component.mapResult(3));
  }
}
