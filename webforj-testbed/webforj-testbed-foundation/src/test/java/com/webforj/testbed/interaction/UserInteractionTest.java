package com.webforj.testbed.interaction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.component.upload.UploadInteraction;
import com.webforj.component.field.TextField;
import com.webforj.component.upload.Upload;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.fixture.CustomPicker;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class UserInteractionTest extends TestbedTestSupport {

  @Test
  void shouldReportTheFocusTheApplicationMoves() {
    TextField name = new TextField();
    TextField email = new TextField();
    List<String> events = new ArrayList<>();
    name.addFocusListener(event -> events.add("focus name"));
    name.addBlurListener(event -> events.add("blur name"));
    email.addFocusListener(event -> events.add("focus email"));
    WebforjTestbed context = WebforjTestbed.getCurrent();
    context.add(name, email);

    name.focus();
    email.focus();

    assertEquals(List.of("focus name", "blur name", "focus email"), events);
    assertTrue(email.hasFocus());
  }

  @Test
  void shouldHoldTheFileTheUserPickedForTheUpload() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Upload upload = new Upload();
    List<Long> sizes = new ArrayList<>();
    upload.onUpload(event -> sizes.add(event.getFiles().get(0).size()));
    context.add(upload);

    context.pickFile("notes.txt", new byte[] {1, 2, 3});
    UserInteraction.perform(upload, UploadInteraction.upload("notes.txt"));

    assertEquals(List.of(3L), sizes);
  }

  @Test
  void shouldReachTheInteractionTheApplicationRegistersAsService() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    CustomPicker picker = new CustomPicker("small", "medium");
    context.add(picker);
    picker.select("medium");

    UserInteraction.close(picker);

    assertEquals(-1, picker.getSelectedIndex());
  }
}
