package org.dwcj.addons.markdown;

import org.dwcj.component.AbstractComponent;
import org.dwcj.component.Component;
import org.dwcj.component.label.Label;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.component.window.Panel;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;


public class DynamicMarkdown extends Panel {

  private final ArrayList<Component> ctrlList = new ArrayList<>();

  @Override
  protected void create(AbstractWindow p) {
    super.create(p);
  }

  @Override
  public DynamicMarkdown setText(String text) {
    super.setText(text);
    redraw(text);
    return this;
  }

  private void redraw(String text) {
    clear();
    ctrlList.clear();

    while (!text.isBlank()) {

      if (text.startsWith("%%%")) {
        String l;
        if (text.contains("\n")) {
          l = text.substring(0, text.indexOf("\n"));
          text = text.substring(text.indexOf("\n"));
        } else {
          l = text;
          text = "";
        }
        l = l.substring(3);

        Class c;
        try {
          c = Class.forName(l);
        } catch (ClassNotFoundException e) {
          Label lbl = new Label("Class " + l + " not found!");
          lbl.setStyle("color", "red");
          add(lbl);
          ctrlList.add(lbl);
          continue;
        }

        AbstractComponent ctrl;
        try {
          ctrl = (AbstractComponent) c.getDeclaredConstructor().newInstance();
        } catch (Exception ex) {
          Label lbl =
              new Label("Class " + l + " cannot be instantiated! Is this a DWCJ control class?");
          lbl.setStyle("color", "red");
          add(lbl);
          ctrlList.add(lbl);
          continue;

        }

        add(ctrl);
        ctrlList.add(ctrl);
        continue;
      }

      String block;
      if (text.contains("%%%")) {
        block = text.substring(0, text.indexOf("%%%"));
        text = text.substring(text.indexOf("%%%"));
      } else {
        block = text;
        text = "";
      }

      block = resolveGitHub(block);

      while (block.contains("&percnt;&percnt;&percnt;"))
        block = block.replace("&percnt;&percnt;&percnt;", "%%%");

      Markdown m = new Markdown();
      add(m);
      m.setText(block);
      ctrlList.add(m);


    }


  }

  private String resolveGitHub(String block) {

    boolean inCode = false;

    String result = new String();
    String[] lines = block.split("\n");
    for (String line : lines) {
      if (line.startsWith("```")) {
        inCode = !inCode;
        result += line + "\n";
        continue;
      }
      if (inCode) {
        result += line + "\n";
        continue;
      }
      if (line.startsWith("https://github.com")) {
        result += "[" + line + "](" + line + " \"View on GitHub\")" + "\n";
        result += "```" + guessLanguage(line) + "\n";
        result += getGitHubSnippet(line) + "\n";
        result += "```\n";
        continue;
      }
      result += line + "\n";
    }
    return result;
  }

  private String guessLanguage(String line) {
    if (line.contains("#")) {
      line = line.substring(0, line.indexOf("#"));
    }
    String[] extensions = line.split("\\.");
    if (extensions.length > 0) {
      // just returning the extension
      // this works well for .java and .bbj but may require later massaging for other extensions
      return extensions[extensions.length - 1];
    } else {
      return "";
    }
  }

  private String getGitHubSnippet(String line) {
    String anchor = "";
    if (line.contains("#")) {
      anchor = line.substring(line.indexOf("#") + 1);
      line = line.substring(0, line.indexOf("#"));
    }
    line = line.replace("https://github.com/", "https://raw.githubusercontent.com/");
    line = line.replace("/blob", "");

    String code;
    URL url = null;
    try {
      url = new URL(line);
    } catch (MalformedURLException e) {
      return "Error: malformed URL " + line;
    }
    try (InputStream in = url.openStream()) {
      byte[] bytes = in.readAllBytes();
      code = new String(bytes);
    } catch (IOException e) {
      return "Error reading from URL " + line;
    }


    if (!anchor.isBlank() && anchor.contains("-") && anchor.startsWith("L")) {
      String[] lines = anchor.split("-");
      int line1 = -1;
      int line2 = -1;
      try {
        line1 = Integer.parseInt(lines[0].substring(1)) - 1;
        line2 = Integer.parseInt(lines[1].substring(1)) - 1;
      } finally {
      }

      if (line1 > 0 && line2 > 0) {
        String[] whole = code.split("\n");
        StringWriter wr = new StringWriter();
        if (whole.length >= line2 && whole.length >= line1) {
          for (int i = Math.min(line1, line2); i < Math.max(line1, line2); i++) {
            wr.append(whole[i]);
            wr.append("\n");
          }
          code = wr.toString();
        }


      }

    }
    return code;
  }

}
