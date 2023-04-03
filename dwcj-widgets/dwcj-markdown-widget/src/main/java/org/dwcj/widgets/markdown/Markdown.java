package org.dwcj.widgets.markdown;

import org.dwcj.util.Assets;
import org.dwcj.widgets.code.Code;
import org.commonmark.node.Node;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.component.window.Panel;
import org.dwcj.environment.ObjectTable;

import java.util.HashSet;

public class Markdown extends Panel {

    public static final String PRISM_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/prism.js";
    public static final String PRISM_CSS = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism-tomorrow.min.css";
    public static final String PRISM_LANG_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-%%language%%.min.js";
    public static final String PRISM_TB_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/toolbar/prism-toolbar.min.js";
    public static final String PRISM_TB_CSS = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/toolbar/prism-toolbar.min.css";
    public static final String PRISM_CLIPBOARD_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/copy-to-clipboard/prism-copy-to-clipboard.min.js";
    private HtmlContainer hv;

    private final HashSet<String> loadedLangs = new HashSet<>();
    private boolean clipboardButtonLoaded=false;
    private HashSet<String> prism_loaded;

    @Override
    protected void create(AbstractWindow p) {
        super.create(p);

        if (!ObjectTable.contains("PRISM_LOADED")){
            prism_loaded = new HashSet<>();
            ObjectTable.put("PRISM_LOADED", prism_loaded);
        } else {
            prism_loaded = (HashSet<String>) ObjectTable.get("PRISM_LOADED");
        }


        hv = new HtmlContainer();
        add(hv);

        loadPrismLib();

    }

    @Override
    public Markdown setText(String text) {
        loadLanguages(text);
        hv.setText(this.getMarkdown(text));
        super.setText(text);
        String scr = "if (typeof Prism != 'undefined'){Prism.highlightAll();}";
        hv.executeScript(scr);
        return this;
    }

    private void loadLanguages(String text) {
        String[] langtags = text.split("\n");
        for (int i=0; i<langtags.length; i++){
            if (langtags[i].startsWith("```")&&langtags[i].length()>3){
                String lang=langtags[i].substring(3);
                    addLanguage(lang);
            }
        }

    }

    public Markdown addLanguage(String language){
        if (Code.isSupportedLanguage(language) && ! loadedLangs.contains(language)) {
            String url = PRISM_LANG_URL.replace("%%language%%", language);

            String scr = "function whenPrismLoaded (callback) { if (typeof Prism === 'undefined') {setTimeout (function () {whenPrismLoaded (callback);}, 100);} else { callback (); }}";
            hv.injectScript(scr);

            scr = "function whenPrismLang" + language + "Loaded (callback) { if (typeof Prism.languages." + language + " === 'undefined' || typeof Prism === 'undefined') {setTimeout (function () {whenPrismLang" + language + "Loaded (callback);}, 100);} else { callback (); }}";
            hv.injectScript(scr);

            scr = "whenPrismLoaded(function() {var link2 =  document.createElement('script');link2.setAttribute('type','module');link2.setAttribute('src','" + url + "');" + "document.head.appendChild(link2);whenPrismLang" + language + "Loaded(function() {Prism.highlightAll();}) });";
            hv.executeScript(scr);
            loadedLangs.add(language);


            if (!this.clipboardButtonLoaded && !prism_loaded.contains(PRISM_TB_URL))  {

                scr = "whenPrismLoaded(function() {var link =  document.createElement('script');link.setAttribute('type','module');link.setAttribute('src','" + PRISM_TB_URL + "');" + "document.head.appendChild(link);"+
                        "var csslink =  document.createElement('link');" + "csslink.setAttribute('rel','stylesheet');" + "csslink.setAttribute('href','" + PRISM_TB_CSS + "');" + "document.head.appendChild(csslink);})";
                hv.executeScript(scr);

                scr = "whenPrismLoaded(function() {var link =  document.createElement('script');link.setAttribute('type','module');link.setAttribute('src','" + PRISM_CLIPBOARD_URL + "');" + "document.head.appendChild(link);})";
                hv.executeScript(scr);
                this.clipboardButtonLoaded=true;

                prism_loaded.add(PRISM_TB_URL);
            }
        }
        return this;
    }


    private void loadPrismLib() {
        if (!prism_loaded.contains(PRISM_URL)) {
            String script = "var link =  document.createElement('script');link.setAttribute('type','module');link.setAttribute('src','" + PRISM_URL + "');" + "document.head.appendChild(link);" + "var csslink =  document.createElement('link');" + "csslink.setAttribute('rel','stylesheet');" + "csslink.setAttribute('href','" + PRISM_CSS + "');" + "document.head.appendChild(csslink);";
            hv.executeScript(script);
            prism_loaded.add(PRISM_URL);
        }
    }

    private String getMarkdown(String code){

        Parser parser = Parser.builder().build();
        Node document = parser.parse(code);
        HtmlRenderer renderer = HtmlRenderer.builder().build();
        return "<html>"+renderer.render(document);  // "<p>This is <em>Sparta</em></p>\n"
    }

    public Markdown load(String filepath){
        setText(Assets.contentOf(filepath));
        return this;
    }



}
