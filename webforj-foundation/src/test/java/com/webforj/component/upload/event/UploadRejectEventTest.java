package com.webforj.component.upload.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.webforj.UploadedFile;
import com.webforj.component.upload.Upload;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class UploadRejectEventTest {

  @Test
  void shouldExposeFileAndMessageFromPayload() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("files", List.of(new UploadedFile("huge.zip")));
    payload.put("message", "too-big");

    UploadRejectEvent ev = new UploadRejectEvent(new Upload(), payload);

    assertNotNull(ev.getFile());
    assertEquals("huge.zip", ev.getFile().getClientName());
    assertEquals("too-big", ev.getMessage());
  }

  @Test
  void shouldReturnDefaultsWhenPayloadFieldsAreMissing() {
    UploadRejectEvent ev = new UploadRejectEvent(new Upload(), new HashMap<>());

    assertNull(ev.getFile());
    assertEquals("", ev.getMessage());
  }
}
