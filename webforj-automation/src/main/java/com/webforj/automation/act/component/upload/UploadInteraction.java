package com.webforj.automation.act.component.upload;

import com.basis.startup.type.BBjVector;
import com.google.auto.service.AutoService;
import com.webforj.UploadedFile;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CancelInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optiondialog.FileUploadI18n;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadCancelEvent;
import com.webforj.component.upload.event.UploadEvent;
import com.webforj.component.upload.event.UploadRejectEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Uploads the files a user picked and cancels uploads.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class UploadInteraction extends AbstractComponentInteraction<Upload>
    implements CancelInteraction<Upload> {

  /**
   * Creates the interaction.
   */
  public UploadInteraction() {
    super(Upload.class);
  }

  /**
   * Returns the interaction that uploads the files with the given names, the way the browser
   * reports the files a user picked. The bytes of the files are what the client holds.
   *
   * <p>
   * A file beyond the maximum count of files, larger than the maximum file size or of a type the
   * active filter does not accept is reported as rejected and is not uploaded.
   * </p>
   *
   * <pre>{@code
   * UserInteraction.perform(upload, UploadInteraction.upload("contacts.csv"))
   * }</pre>
   *
   * @param names the names of the files on the client
   *
   * @return the interaction to perform on the upload
   * @throws NullPointerException when performed, if a name is null
   * @throws IllegalArgumentException when performed, if no file or a file without name is given
   */
  public static Consumer<Upload> upload(String... names) {
    return upload -> {
      if (names.length == 0) {
        throw new IllegalArgumentException("A user uploads at least one file");
      }

      for (String name : names) {
        Objects.requireNonNull(name, "The file name must not be null");
        if (name.isBlank()) {
          throw new IllegalArgumentException("The file name must not be blank");
        }
      }

      List<String> picked = new ArrayList<>();
      for (String name : names) {
        Optional<String> rejection = findRejection(upload, name, picked.size());
        if (rejection.isPresent()) {
          Map<String, Object> data = new HashMap<>();
          data.put("files", List.of(new UploadedFile(name)));
          data.put("message", rejection.get());
          UserInteraction.fireEvent(new UploadRejectEvent(upload, data));
        } else {
          picked.add(name);
        }
      }

      if (picked.isEmpty()) {
        return;
      }

      Map<String, Object> data = new HashMap<>();
      data.put("files", new BBjVector(picked));
      UserInteraction.fireEvent(new UploadEvent(upload, data));
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void cancel(Upload upload) {
    if (!upload.isVisible(Upload.Part.CANCEL_BUTTON)) {
      throw reject(upload, "has no cancel button and a user cannot cancel it");
    }

    fire(new UploadCancelEvent(upload, new HashMap<>()));
  }

  private static Optional<String> findRejection(Upload upload, String name, int picked) {
    FileUploadI18n i18n = upload.getI18n();
    if (picked >= getMaxFiles(upload)) {
      return Optional.of(i18n.getStatusMaxed());
    }

    Number maxFileSize = upload.getMaxFileSize();
    if (maxFileSize != null && new UploadedFile(name).size() > maxFileSize.doubleValue()) {
      return Optional.of(i18n.getStatusTooBig());
    }

    if (!isAcceptedType(upload, name)) {
      return Optional.of(i18n.getStatusNotAllowedType());
    }

    return Optional.empty();
  }

  private static double getMaxFiles(Upload upload) {
    if (upload.getMaxFiles() != null) {
      return upload.getMaxFiles().doubleValue();
    }

    return upload.getSelectionMode() == Upload.SelectionMode.SINGLE ? 1 : Double.POSITIVE_INFINITY;
  }

  private static boolean isAcceptedType(Upload upload, String name) {
    List<FileChooserFilter> filters = upload.getFilters();
    if (upload.isAllFilesFilterEnabled() || filters.isEmpty()) {
      return true;
    }

    FileChooserFilter active =
        upload.getActiveFilter() == null ? filters.get(0) : upload.getActiveFilter();
    String accepted = Arrays.stream(active.getPattern().split(";")).map(String::trim)
        .filter(glob -> !glob.isEmpty())
        .map(glob -> glob.startsWith("*") && glob.length() > 1 ? glob.substring(1) : glob)
        .collect(Collectors.joining("|"));
    int dot = name.lastIndexOf('.');
    String extension = dot < 0 ? "" : name.substring(dot);

    return Pattern.compile("^(" + accepted + ")$", Pattern.CASE_INSENSITIVE).matcher(extension)
        .matches();
  }
}
