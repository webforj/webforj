package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.optiondialog.MessageDialog.DefaultButton;
import com.webforj.optiondialog.MessageDialog.IconType;
import com.webforj.optiondialog.MessageDialog.MessageType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MessageDialogTest {

  private MessageDialog dialog;

  @BeforeEach
  void setUp() {
    dialog = new MessageDialog();
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
    dialog.setMessageType(MessageType.OK_CANCEL);
    assertEquals(MessageType.OK_CANCEL, dialog.getMessageType());
  }

  @Test
  void shouldSetAndGetIconTypeCorrectly() {
    dialog.setIconType(IconType.QUESTION);
    assertEquals(IconType.QUESTION, dialog.getIconType());
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
