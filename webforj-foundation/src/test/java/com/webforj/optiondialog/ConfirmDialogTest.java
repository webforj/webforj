package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.optiondialog.ConfirmDialog.DefaultButton;
import com.webforj.optiondialog.ConfirmDialog.MessageType;
import com.webforj.optiondialog.ConfirmDialog.OptionType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ConfirmDialogTest {

  private ConfirmDialog dialog;

  @BeforeEach
  void setUp() {
    dialog = new ConfirmDialog();
  }

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

  @Test
  void shouldSetAndGetTitleCorrectly() {
    dialog.setTitle("Test Title");
    assertEquals("Test Title", dialog.getTitle());
  }

  @Test
  void shouldSetAndGetMessageCorrectly() {
    dialog.setMessage("Test Message");
    assertEquals("Test Message", dialog.getMessage());
  }

  @Test
  void shouldSetAndGetTypeOfMessageCorrectly() {
    dialog.setOptionType(OptionType.OK_CANCEL);
    assertEquals(OptionType.OK_CANCEL, dialog.getOptionType());
  }

  @Test
  void shouldSetAndGetIconTypeCorrectly() {
    dialog.setMessageType(MessageType.QUESTION);
    assertEquals(MessageType.QUESTION, dialog.getMessageType());
  }

  @Test
  void shouldSetAndGetDefaultButtonCorrectly() {
    dialog.setDefaultButton(DefaultButton.SECOND);
    assertEquals(DefaultButton.SECOND, dialog.getDefaultButton());
  }

  @Test
  void shouldEnableAndCheckRawTextCorrectly() {
    dialog.setRawText(true);
    assertTrue(dialog.isRawText());
  }

  @Test
  void shouldSetAndGetButtonTimeoutCorrectly() {
    dialog.setTimeout(300);
    assertEquals(300, dialog.getTimeout());
  }
}
