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

class UploadErrorEventTest {

  @Test
  void shouldExposeFileAndMessageFromPayload() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("files", List.of(new UploadedFile("doc.pdf")));
    payload.put("message", "network timeout");

    UploadErrorEvent ev = new UploadErrorEvent(new Upload(), payload);

    assertNotNull(ev.getFile());
    assertEquals("doc.pdf", ev.getFile().getClientName());
    assertEquals("network timeout", ev.getMessage());
  }

  @Test
  void shouldReturnDefaultsWhenPayloadFieldsAreMissing() {
    UploadErrorEvent ev = new UploadErrorEvent(new Upload(), new HashMap<>());

    assertNull(ev.getFile());
    assertEquals("", ev.getMessage());
  }
}
