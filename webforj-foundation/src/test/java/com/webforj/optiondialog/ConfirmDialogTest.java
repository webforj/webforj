package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.optiondialog.ConfirmDialog.DefaultButton;
import com.webforj.optiondialog.ConfirmDialog.MessageType;
import com.webforj.optiondialog.ConfirmDialog.OptionType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ConfirmDialogTest {

  private ConfirmDialog dialog;

  @BeforeEach
  void setUp() {
    dialog = new ConfirmDialog();
  }

  @Nested
  class Constructors {
    @Test
    void shouldCreateMessageDialogWithAllParameters() {
      ConfirmDialog dialog =
          new ConfirmDialog("Message", "Title", OptionType.YES_NO, MessageType.INFORMATION);
      assertNotNull(dialog);
      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals(OptionType.YES_NO, dialog.getOptionType());
      assertEquals(MessageType.INFORMATION, dialog.getMessageType());
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
    dialog.setTitle("Test Title");
    assertEquals("Test Title", dialog.getTitle());
  }

  @Test
  void shouldSetAndGetMessage() {
    dialog.setMessage("Test Message");
    assertEquals("Test Message", dialog.getMessage());
  }

  @Test
  void shouldSetAndGetOptionTypeOfMessage() {
    dialog.setOptionType(OptionType.OK_CANCEL);
    assertEquals(OptionType.OK_CANCEL, dialog.getOptionType());
  }

  @Test
  void shouldSetAndGetMessageType() {
    dialog.setMessageType(MessageType.QUESTION);
    assertEquals(MessageType.QUESTION, dialog.getMessageType());
  }

  @Test
  void shouldSetAndGetDefaultButton() {
    dialog.setDefaultButton(DefaultButton.SECOND);
    assertEquals(DefaultButton.SECOND, dialog.getDefaultButton());
  }

  @Test
  void shouldEnableAndCheckRawText() {
    dialog.setRawText(true);
    assertTrue(dialog.isRawText());
  }

  @Test
  void shouldSetAndGetFirstButtonText() {
    dialog.setButtonText("First");
    assertEquals("First", dialog.getFirstButtonText());
  }

  @Test
  void shouldSetAndGetSecondButtonText() {
    dialog.setButtonText("First", "Second");
    assertEquals("Second", dialog.getSecondButtonText());
  }

  @Test
  void shouldSetAndGetThirdButtonText() {
    dialog.setButtonText("First", "Second", "Third");
    assertEquals("Third", dialog.getThirdButtonText());
  }

  @Test
  void shouldSetAndGetButtonTimeout() {
    dialog.setTimeout(300);
    assertEquals(300, dialog.getTimeout());
  }

  @Test
  void shouldMapResult() {
    assertEquals(ConfirmDialog.Result.OK, dialog.mapResult(1));
    assertEquals(ConfirmDialog.Result.CANCEL, dialog.mapResult(2));
    assertEquals(ConfirmDialog.Result.ABORT, dialog.mapResult(3));
    assertEquals(ConfirmDialog.Result.RETRY, dialog.mapResult(4));
    assertEquals(ConfirmDialog.Result.IGNORE, dialog.mapResult(5));
    assertEquals(ConfirmDialog.Result.YES, dialog.mapResult(6));
    assertEquals(ConfirmDialog.Result.NO, dialog.mapResult(7));
    assertEquals(ConfirmDialog.Result.TIMEOUT, dialog.mapResult(-1));

    dialog.setOptionType(OptionType.CUSTOM);
    assertEquals(ConfirmDialog.Result.FIRST_CUSTOM_BUTTON, dialog.mapResult(1));
    assertEquals(ConfirmDialog.Result.SECOND_CUSTOM_BUTTON, dialog.mapResult(2));
    assertEquals(ConfirmDialog.Result.THIRD_CUSTOM_BUTTON, dialog.mapResult(3));
  }
}
