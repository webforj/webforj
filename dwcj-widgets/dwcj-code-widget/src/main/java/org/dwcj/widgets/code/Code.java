package org.dwcj.widgets.code;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.panels.AbstractPanel;
import org.dwcj.component.panels.Div;
import org.dwcj.environment.ObjectTable;

import java.util.HashSet;

public class Code  extends Div {

    public static final String PRISM_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/prism.js";
    public static final String PRISM_CSS = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/themes/prism-tomorrow.min.css";
    public static final String PRISM_LANG_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/components/prism-%%language%%.min.js";

    public static final String PRISM_TB_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/toolbar/prism-toolbar.min.js";
    public static final String PRISM_TB_CSS = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/toolbar/prism-toolbar.min.css";
    public static final String PRISM_CLIPBOARD_URL = "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/plugins/copy-to-clipboard/prism-copy-to-clipboard.min.js";

    private HtmlContainer hv;

    private String language;
    private boolean clipboardButtonLoaded;
    private HashSet<String> prism_loaded;

    @Override
    protected void create(AbstractPanel p) {
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
    public Code setText(String text) {
        text=text.replace("<","&lt;");
        text=text.replace(">","&gt;");
        hv.setText("<html><pre><code class='language-"+language+"'>"+text+"</code></pre>");
        super.setText(text);
        String scr = "if (typeof Prism != 'undefined' && typeof whenPrismLang"+language+"Loaded != 'undefined') {whenPrismLang"+language+"Loaded(function() {Prism.highlightAll();})}";
        hv.executeScript(scr);
        return this;
    }

    public Code setLanguage(String language){
        if (isSupportedLanguage(language)) {
            this.language = language;
            this.setText(super.getText());
            String url = PRISM_LANG_URL.replace("%%language%%", language);

            String scr = "function whenPrismLoaded (callback) { if (typeof Prism === 'undefined') {setTimeout (function () {whenPrismLoaded (callback);}, 100);} else { callback (); }}";
            hv.injectScript(scr);

            scr = "function whenPrismLang" + language + "Loaded (callback) { if (typeof Prism.languages." + language + " === 'undefined' || typeof Prism === 'undefined') {setTimeout (function () {whenPrismLang" + language + "Loaded (callback);}, 100);} else { callback (); }}";
            hv.injectScript(scr);

            scr = "whenPrismLoaded(function() {var link2 =  document.createElement('script');link2.setAttribute('type','module');link2.setAttribute('src','" + url + "');" + "document.head.appendChild(link2);whenPrismLang" + language + "Loaded(function() {Prism.highlightAll();}) });";
            hv.executeScript(scr);

            if (!this.clipboardButtonLoaded && !prism_loaded.contains(PRISM_TB_URL)) {

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

        String script = "var link =  document.createElement('script');link.setAttribute('type','module');link.setAttribute('src','" + PRISM_URL + "');" + "document.head.appendChild(link);"+
                        "var csslink =  document.createElement('link');" + "csslink.setAttribute('rel','stylesheet');" + "csslink.setAttribute('href','" + PRISM_CSS + "');" + "document.head.appendChild(csslink);";
        hv.executeScript(script);



    }

    public static Boolean isSupportedLanguage(String language){
        //TODO: find a more solid way to determine which languages require additional loading
        String prism_langs = "+markdown*+java*+plain*+plaintext*+text*+txt*+html*+xml*+svg*+mathml*+ssml*+rss*+css*+clike*+js*+abap*+abnf*+al*+antlr4*+g4*+apacheconf*+apl*+aql*+ino*+arff*+armasm*+arm-asm*+art*+asciidoc*+adoc*+aspnet*+asm6502*+asmatmel*+autohotkey*+autoit*+avisynth*+avs*+avro-idl*+avdl*+awk*+gawk*+sh*+basic*+bbcode*+bbj*+bnf*+rbnf*+bqn*+bsl*+oscript*+csharp*+cs*+dotnet*+cpp*+cfscript*+cfc*+cil*+cilkc*+cilk-c*+cilkcpp*+cilk-cpp*+cilk*+cmake*+cobol*+coffee*+conc*+csp*+css-extras*+csv*+cue*+dataweave*+dax*+django*+jinja2*+dns-zone-file*+dns-zone*+dockerfile*+dot*+gv*+ebnf*+editorconfig*+ejs*+etlua*+erb*+excel-formula*+xlsx*+xls*+fsharp*+firestore-security-rules*+ftl*+gml*+gamemakerlanguage*+gap*+gcode*+gdscript*+gedcom*+gettext*+po*+glsl*+gn*+gni*+linker-script*+ld*+go-module*+go-mod*+graphql*+hbs*+hs*+hcl*+hlsl*+http*+hpkp*+hsts*+ichigojam*+icu-message-format*+idr*+ignore*+gitignore*+hgignore*+npmignore*+inform7*+javadoc*+javadoclike*+javastacktrace*+jq*+jsdoc*+js-extras*+json*+webmanifest*+json5*+jsonp*+jsstacktrace*+js-templates*+keepalived*+kts*+kt*+kumir*+kum*+latex*+tex*+context*+lilypond*+ly*+emacs*+elisp*+emacs-lisp*+llvm*+log*+lolcode*+magma*+md*+markup-templating*+matlab*+maxscript*+mel*+metafont*+mongodb*+moon*+n1ql*+n4js*+n4jsd*+nand2tetris-hdl*+naniscript*+nani*+nasm*+neon*+nginx*+nsis*+objectivec*+objc*+ocaml*+opencl*+openqasm*+qasm*+parigp*+objectpascal*+psl*+pcaxis*+px*+peoplecode*+pcode*+php*+phpdoc*+php-extras*+plant-uml*+plantuml*+plsql*+powerquery*+pq*+mscript*+powershell*+promql*+properties*+protobuf*+purebasic*+pbfasm*+purs*+py*+qsharp*+qs*+q*+qml*+rkt*+cshtml*+razor*+jsx*+tsx*+renpy*+rpy*+res*+rest*+robotframework*+robot*+rb*+sas*+sass*+scss*+shell-session*+sh-session*+shellsession*+sml*+smlnj*+solidity*+sol*+solution-file*+sln*+soy*+sparql*+rq*+splunk-spl*+sqf*+sql*+stata*+iecst*+supercollider*+sclang*+systemd*+t4-templating*+t4-cs*+t4*+t4-vb*+tap*+tt2*+toml*+trickle*+troy*+trig*+ts*+tsconfig*+uscript*+uc*+uorazor*+uri*+url*+vbnet*+vhdl*+vim*+visual-basic*+vba*+vb*+wasm*+web-idl*+webidl*+wgsl*+wiki*+wolfram*+nb*+wl*+xeoracube*+xml-doc*+xojo*+xquery*+yaml*+yml*+yang*";
        return prism_langs.contains("+"+language+"*");
    }
}
