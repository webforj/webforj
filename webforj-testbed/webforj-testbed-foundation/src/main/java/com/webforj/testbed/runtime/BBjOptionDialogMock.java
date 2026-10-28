package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.webforj.testbed.ShownOptionDialog;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * Mock of the blocking option dialogs, which record what the application asks for and return the
 * scripted answer when shown.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class BBjOptionDialogMock extends RuntimeMock {
  private final ShownOptionDialog.Kind kind;
  private final OptionDialogAnswerRegistry answers;
  private final Map<String, Object> settings = new HashMap<>();

  BBjOptionDialogMock(ShownOptionDialog.Kind kind, OptionDialogAnswerRegistry answers) {
    this.kind = kind;
    this.answers = answers;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) {
    if (method.getName().equals("show")) {
      return show();
    }

    if (args.length == 1) {
      // Every configuration method of a dialog takes one value and returns the dialog for chaining.
      settings.put(method.getName(), args[0]);

      return proxy;
    }

    return UNHANDLED;
  }

  private Object show() {
    ShownOptionDialog.Kind shownKind = shownKind();
    ShownOptionDialog request = new ShownOptionDialog(shownKind, text("title"), text("message"));
    if (shownKind == ShownOptionDialog.Kind.CONFIRM) {
      return answers.confirm(request, isSingleButton());
    }

    return answers.text(request, scriptedBy(shownKind));
  }

  // The file chooser and the file upload dialog open the same runtime dialog, the upload dialog
  // marks itself through the CLIENT mode.
  private ShownOptionDialog.Kind shownKind() {
    String modes = text("modes");
    if (kind == ShownOptionDialog.Kind.FILE_CHOOSER && modes != null
        && modes.startsWith("CLIENT")) {
      return ShownOptionDialog.Kind.FILE_UPLOAD;
    }

    return kind;
  }

  // A message dialog is a confirm dialog with one button, the user can only dismiss it, so it needs
  // no scripted answer.
  private boolean isSingleButton() {
    int options = settings.get("options") instanceof Integer value ? value : 0;
    int buttons = options & 7;
    if (buttons == SysGuiProxyConstants.MSGBOX_BUTTONS_OK) {
      return true;
    }

    return buttons == SysGuiProxyConstants.MSGBOX_BUTTONS_CUSTOM && !settings.containsKey("button2")
        && !settings.containsKey("button3");
  }

  private static String scriptedBy(ShownOptionDialog.Kind kind) {
    return switch (kind) {
      case INPUT -> "WebforjTestbed.getCurrent().answerInputDialog";
      case FILE_CHOOSER -> "WebforjTestbed.getCurrent().answerFileChooserDialog";
      case FILE_SAVE -> "WebforjTestbed.getCurrent().answerFileSaveDialog";
      case FILE_UPLOAD -> "WebforjTestbed.getCurrent().answerFileUploadDialog";
      default -> "WebforjTestbed.getCurrent().answerConfirmDialog";
    };
  }

  private String text(String setting) {
    Object value = settings.get(setting);

    return value == null ? null : String.valueOf(value);
  }
}
