package org.dwcj.addons.terminal;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerJavascriptEvent;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.component.window.Panel;
import org.dwcj.addons.terminal.events.TerminalKeyEvent;

import java.util.ArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;

public class Terminal extends Panel {


  private HtmlContainer hv;
  protected final String uuid = java.util.UUID.randomUUID().toString();

  private final LinkedBlockingQueue<String> keyBuffer = new LinkedBlockingQueue<>();

  private final ArrayList<Consumer<TerminalKeyEvent>> callbacks = new ArrayList<>();

  @Override
  protected void create(AbstractWindow p) {
    super.create(p);

    hv = new HtmlContainer("<div id='" + uuid + "'></div>");
    hv.setStyle("flex", "1");
    add(hv);


    String script = "function send(event){var custom=new CustomEvent("
        + "'custom_event',{bubbles:true,cancelable:true});" + "custom.key=event.key;"
        + "window.basisDispatchCustomEvent(event.target,custom);}";
    hv.injectScript(script, true);

    script =
        "function whenTerminalLoaded (callback) { if (typeof Terminal === 'undefined' ) {setTimeout (function () {whenTerminalLoaded (callback);}, 100);} else { callback (); }}";
    hv.injectScript(script);

    script = "var link =  $doc.createElement('script');"
        + "link.setAttribute('type','text/javascript');"
        + "link.setAttribute('src','https://cdn.jsdelivr.net/npm/xterm@4.13.0/lib/xterm.js');"
        + "document.head.appendChild(link);" + "var link2 =  $doc.createElement('link');"
        + "link2.setAttribute('rel','stylesheet');"
        + "link2.setAttribute('href','https://cdn.jsdelivr.net/npm/xterm@4.13.0/css/xterm.css');"
        + "document.head.appendChild(link2);";
    hv.executeScript(script);

    script = "whenTerminalLoaded(function() {window.term = new window.Terminal();"
        + "window.term.open(document.getElementById('" + uuid + "'));"
        + "window.term.write('\\x1B[1;3;31mDWCJ\\x1B[0m Terminal');" + "window.term.focus();"
        + "window.term.onKey(" + "function ({key, domEvent}) " + "{" + "send(domEvent);" + "})})";
    hv.executeScript(script);

    hv.onJavascriptEvent(this::onJS);
  }

  private void onJS(HtmlContainerJavascriptEvent htmlContainerJavascriptEvent) {
    String a = htmlContainerJavascriptEvent.getEventMap().get("key");
    keyBuffer.add(a);

    TerminalKeyEvent event = new TerminalKeyEvent(this, htmlContainerJavascriptEvent.getEventMap());

    for (Consumer<TerminalKeyEvent> c : callbacks) {
      c.accept(event);
    }

  }

  public Terminal onKey(Consumer<TerminalKeyEvent> callback) {
    callbacks.add(callback);
    return this;
  }

  public Terminal write(String chunk) {

    if (chunk.contains("'"))
      chunk = chunk.replace("'", "\\'");

    if (chunk.contains(")"))
      chunk = chunk.replace(")", "\\)");

    if (chunk.contains("\r"))
      chunk = chunk.replace("\r", "\\r");

    if (chunk.contains("\n"))
      chunk = chunk.replace("\n", "\\n\\r");


    String script = "whenTerminalLoaded(function() {window.term.write('" + chunk + "');})";
    hv.executeScript(script);
    return this;
  }
}
