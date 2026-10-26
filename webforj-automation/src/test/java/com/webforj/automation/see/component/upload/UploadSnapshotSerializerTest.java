package com.webforj.automation.see.component.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.upload.Upload;
import java.util.List;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class UploadSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldFlagTheUploadOfSeveralFilesAtBothLevels(boolean browserState) {
    Upload upload = new Upload(List.of(new FileChooserFilter("Images", "*.png")));
    upload.setSelectionMode(Upload.SelectionMode.MULTIPLE);
    upload.setDrop(false);

    assertEquals("""
        - Upload [multiple]:
          - Filter "Images" [pattern="*.png"]
        """, Snapshot.render(upload, new SnapshotOptions().setBrowserState(browserState)));
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldLeaveTheUploadOfOneFileUnflaggedAtBothLevels(boolean browserState) {
    Upload upload = new Upload();
    upload.setSelectionMode(Upload.SelectionMode.SINGLE);
    upload.setDrop(false);

    assertEquals("- Upload\n",
        Snapshot.render(upload, new SnapshotOptions().setBrowserState(browserState)));
  }
}
