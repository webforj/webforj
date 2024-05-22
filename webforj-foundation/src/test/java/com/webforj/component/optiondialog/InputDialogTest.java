package com.webforj.component.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.optiondialog.DwcPromptMsgBox.MessageType;
import com.webforj.component.optiondialog.InputDialog.Button;
import com.webforj.component.optiondialog.InputDialog.InputType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class InputDialogTest {

  private InputDialog component;

  @BeforeEach
  void setUp() {
    component = new InputDialog();
  }

  @Nested
  class Constructors {

    @Test
    void shouldCreateInputDialogWithAllParameters() {
      InputDialog dialog = new InputDialog("Message", "Title", "Default", MessageType.INFO,
          InputType.TEXT, Theme.DANGER);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Default", dialog.getDefaultValue());
      assertEquals(MessageType.INFO, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DANGER, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleDefaultValueMessageAndInputType() {
      InputDialog dialog =
          new InputDialog("Message", "Title", "Default", MessageType.INFO, InputType.TEXT);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Default", dialog.getDefaultValue());
      assertEquals(MessageType.INFO, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.INFO, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleDefaultValueAndMessageType() {
      InputDialog dialog = new InputDialog("Message", "Title", "Default", MessageType.QUESTION);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Default", dialog.getDefaultValue());
      assertEquals(MessageType.QUESTION, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.PRIMARY, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleDefaultValueAndInputType() {
      InputDialog dialog = new InputDialog("Message", "Title", "Default", InputType.PASSWORD);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Default", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.PASSWORD, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleMessageAndInputType() {
      InputDialog dialog =
          new InputDialog("Message", "Title", MessageType.ERROR, InputType.PASSWORD);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.ERROR, dialog.getMessageType());
      assertEquals(InputType.PASSWORD, dialog.getType());
      assertEquals(Theme.DANGER, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleAndMessageType() {
      InputDialog dialog = new InputDialog("Message", "Title", MessageType.ERROR);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.ERROR, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DANGER, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleAndInputType() {
      InputDialog dialog = new InputDialog("Message", "Title", InputType.COLOR);

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.COLOR, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageMessageAndInputType() {
      InputDialog dialog = new InputDialog("Message", MessageType.ERROR, InputType.DATE);

      assertEquals(InputDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.ERROR, dialog.getMessageType());
      assertEquals(InputType.DATE, dialog.getType());
      assertEquals(Theme.DANGER, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageAndMessageType() {
      InputDialog dialog = new InputDialog("Message", MessageType.ERROR);

      assertEquals(InputDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.ERROR, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DANGER, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageAndInputType() {
      InputDialog dialog = new InputDialog("Message", InputType.EMAIL);

      assertEquals(InputDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.EMAIL, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageTitleAndDefaultValue() {
      InputDialog dialog = new InputDialog("Message", "Title", "Default");

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("Default", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessageAndTitle() {
      InputDialog dialog = new InputDialog("Message", "Title");

      assertEquals("Title", dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }

    @Test
    void shouldCreateInputDialogWithMessage() {
      InputDialog dialog = new InputDialog("Message");

      assertEquals(InputDialog.DEFAULT_TITLE, dialog.getTitle());
      assertEquals("Message", dialog.getMessage());
      assertEquals("", dialog.getDefaultValue());
      assertEquals(MessageType.PLAIN, dialog.getMessageType());
      assertEquals(InputType.TEXT, dialog.getType());
      assertEquals(Theme.DEFAULT, dialog.getTheme());
    }
  }

  @ParameterizedTest
  @EnumSource(InputType.class)
  void shouldSetType(InputType type) {
    InputDialog dialog = new InputDialog("Message").setType(type);

    assertEquals(type, dialog.getType());
  }

  @Test
  void shouldSetAndGetButtonsTexts() {
    component.setButtonText(Button.FIRST, "First");
    component.setButtonText(Button.SECOND, "Second");

    assertEquals("First", component.getFirstButtonText());
    assertEquals("Second", component.getSecondButtonText());
  }

  @Test
  void shouldSetAndGetButtonThemes() {
    component.setButtonTheme(Button.FIRST, ButtonTheme.DANGER);
    component.setButtonTheme(Button.SECOND, ButtonTheme.SUCCESS);

    assertEquals("\"danger\"", component.getAttributes().get("button-0-theme"));
    assertEquals("\"success\"", component.getAttributes().get("button-1-theme"));
  }
}
