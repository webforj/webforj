package com.webforj.automation.act.component.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.UploadedFile;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.upload.Upload;
import com.webforj.component.window.Frame;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class UploadInteractionTest {

  @Test
  void shouldUploadTheFilesTheUserPicked() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      List<String> names = new ArrayList<>();
      upload.onUpload(event -> event.getFiles().forEach(file -> names.add(file.getClientName())));
      frame.add(upload);

      UserInteraction.perform(upload, UploadInteraction.upload("contacts.csv", "notes.txt"));

      assertEquals(List.of("contacts.csv", "notes.txt"), names);
    }
  }

  @Test
  void shouldRefuseAnUploadWithoutFilesOrWithTheFileWithoutName() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      List<String> events = new ArrayList<>();
      upload.onUpload(event -> events.add("upload"));
      frame.add(upload);

      Consumer<Upload> uploadNothing = UploadInteraction.upload();
      Consumer<Upload> uploadBlank = UploadInteraction.upload(" ");
      Consumer<Upload> uploadNull = UploadInteraction.upload((String) null);
      assertThrows(IllegalArgumentException.class,
          () -> UserInteraction.perform(upload, uploadNothing));
      assertThrows(IllegalArgumentException.class,
          () -> UserInteraction.perform(upload, uploadBlank));
      assertThrows(NullPointerException.class, () -> UserInteraction.perform(upload, uploadNull));
      assertEquals(List.of(), events);
    }
  }

  @Test
  void shouldCancelTheUpload() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      upload.setVisible(true, Upload.Part.CANCEL_BUTTON);
      List<String> events = new ArrayList<>();
      upload.onCancel(event -> events.add("cancel"));
      frame.add(upload);

      UserInteraction.cancel(upload);

      assertEquals(List.of("cancel"), events);
    }
  }

  @Test
  void shouldRejectCancellingTheUploadWithoutCancelButton() throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      List<String> events = new ArrayList<>();
      upload.onCancel(event -> events.add("cancel"));
      frame.add(upload);

      assertTrue(
          assertThrows(InteractionRejectedException.class, () -> UserInteraction.cancel(upload))
              .getMessage().contains("no cancel button"));
      assertEquals(List.of(), events);
    }
  }

  @ParameterizedTest
  @MethodSource("provideLimitsOfTheUpload")
  void shouldRejectThePickedFilesOutsideTheLimitsOfTheUpload(Consumer<Upload> limits,
      List<String> names, List<String> expected) throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      limits.accept(upload);
      List<String> events = new ArrayList<>();
      upload.onReject(event -> events
          .add("reject " + event.getFile().getClientName() + " " + event.getMessage()));
      upload.onUpload(event -> events.add("upload " + String.join(", ",
          event.getFiles().stream().map(UploadedFile::getClientName).toList())));
      frame.add(upload);

      UserInteraction.perform(upload, UploadInteraction.upload(names.toArray(String[]::new)));

      assertEquals(expected, events);
    }
  }

  @Test
  void shouldRejectThePickedFileLargerThanTheMaximumFileSize() throws Exception {
    Environment environment = FrameEnvironment.create(null);
    when(environment.getBBjAPI().getThinClient().getClientFileSystem().getClientFile("scan.pdf")
        .size()).thenReturn(1025L);
    when(environment.getBBjAPI().getThinClient().getClientFileSystem().getClientFile("notes.pdf")
        .size()).thenReturn(1024L);

    try (MockedStatic<Environment> current = mockStatic(Environment.class);
        MockedStatic<App> app = mockStatic(App.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame frame = new Frame("Orders");
      app.when(App::getFrames).thenReturn(List.of(frame));
      Upload upload = new Upload();
      upload.setMaxFileSize(1024);
      List<String> events = new ArrayList<>();
      upload.onReject(event -> events
          .add("reject " + event.getFile().getClientName() + " " + event.getMessage()));
      upload.onUpload(event -> events.add("upload " + event.getFiles().size()));
      frame.add(upload);

      UserInteraction.perform(upload, UploadInteraction.upload("scan.pdf", "notes.pdf"));

      assertEquals(List.of("reject scan.pdf File is too big.", "upload 1"), events);
    }
  }

  private static Stream<Arguments> provideLimitsOfTheUpload() {
    Consumer<Upload> images =
        upload -> upload.addFilter("Images", "*.png;*.jpg").setAllFilesFilterEnabled(false);
    Consumer<Upload> imagesOrAllFiles = upload -> upload.addFilter("Images", "*.png;*.jpg");
    Consumer<Upload> documents =
        upload -> upload.addFilter("Images", "*.png").addFilter("Documents", "*.pdf")
            .setAllFilesFilterEnabled(false).setActiveFilter("Documents");
    Consumer<Upload> twoFiles = upload -> upload.setMaxFiles(2);
    Consumer<Upload> single = upload -> upload.setSelectionMode(Upload.SelectionMode.SINGLE);
    Consumer<Upload> singleWithTwoFiles =
        upload -> upload.setSelectionMode(Upload.SelectionMode.SINGLE).setMaxFiles(2);

    return Stream.of(
        Arguments.of(images, List.of("photo.PNG", "notes.txt", "README"),
            List.of("reject notes.txt File type is not allowed.",
                "reject README File type is not allowed.", "upload photo.PNG")),
        Arguments.of(images, List.of("notes.txt"),
            List.of("reject notes.txt File type is not allowed.")),
        Arguments.of(imagesOrAllFiles, List.of("notes.txt"), List.of("upload notes.txt")),
        Arguments.of(documents, List.of("photo.png", "report.pdf"),
            List.of("reject photo.png File type is not allowed.", "upload report.pdf")),
        Arguments.of(twoFiles, List.of("a.csv", "b.csv", "c.csv"),
            List.of("reject c.csv The max amount of allowed files has been reached",
                "upload a.csv, b.csv")),
        Arguments.of(single, List.of("a.csv", "b.csv"),
            List.of("reject b.csv The max amount of allowed files has been reached",
                "upload a.csv")),
        Arguments.of(singleWithTwoFiles, List.of("a.csv", "b.csv"),
            List.of("upload a.csv, b.csv")));
  }
}
