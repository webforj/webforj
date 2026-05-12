package com.webforj.component.upload.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.UploadedFile;
import com.webforj.component.upload.Upload;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class UploadCompleteEventTest {

  @Test
  void shouldExposeUploadedAndFailedFiles() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("uploaded", List.of(new UploadedFile("a.txt"), new UploadedFile("b.txt")));
    payload.put("failed", List.of(new UploadedFile("c.txt")));

    UploadCompleteEvent ev = new UploadCompleteEvent(new Upload(), payload);

    assertEquals(2, ev.getUploadedFiles().size());
    assertEquals("a.txt", ev.getUploadedFiles().get(0).getClientName());
    assertEquals("b.txt", ev.getUploadedFiles().get(1).getClientName());
    assertEquals(1, ev.getFailedFiles().size());
    assertEquals("c.txt", ev.getFailedFiles().get(0).getClientName());
  }

  @Test
  void shouldReturnEmptyListsWhenPayloadIsMissing() {
    UploadCompleteEvent ev = new UploadCompleteEvent(new Upload(), new HashMap<>());

    assertTrue(ev.getUploadedFiles().isEmpty());
    assertTrue(ev.getFailedFiles().isEmpty());
  }
}
