package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.List;

public final class MultilineEdit extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjCEdit bbjCEdit;


    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText());
            catchUp();
            bbjCEdit = (BBjCEdit) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public MultilineEdit addParagraph(int index, String paragraph) {
        try {
            bbjCEdit.addParagraph(index, paragraph);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public void addParagraphs(int index, ArrayList<String> paragraphs) {
        try {
            bbjCEdit.addParagraphs(index, (BBjVector) paragraphs);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void appendToParagraph(int parNum, String text) {
        try {
            bbjCEdit.appendToParagraph(parNum, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public List<String> getAllParagraphs() {
        try {
            return bbjCEdit.getAllParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public int getCurrentParagraphIndex() {
        try {
            return bbjCEdit.getCurrentParagraphIndex();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getHorizontalScrollable() {
        try {
            return bbjCEdit.getHorizontalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getIgnoreEnters() {
        try {
            return bbjCEdit.getIgnoreEnters();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getIgnoreTabs() {
        try {
            return bbjCEdit.getIgnoreTabs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getLimitToOneParagraph() {
        try {
            return bbjCEdit.getLimitToOneParagraph();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getLineCountLimit() {
        try {
            return bbjCEdit.getLineCountLimit();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getLineWrap() {
        try {
            return bbjCEdit.getLineWrap();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getMaxParagraphSize() {
        try {
            return bbjCEdit.getMaxParagraphSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMaxLength() {
        return bbjCEdit.getMaxLength();
    }

    public int getNumberOfParagraphs() {
        try {
            return bbjCEdit.getNumberOfParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getOvertypeMode() {
        try {
            return bbjCEdit.getOvertypeMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getParagraph(int parNum) {
        try {
            return bbjCEdit.getParagraph(parNum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public List<String> getSelection() {
        try {
            return bbjCEdit.getSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public int getTabSize() {
        try {
            return bbjCEdit.getTabSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getVerticalScrollable() {
        try {
            return bbjCEdit.getVerticalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getWrapStyleWord() {
        try {
            return bbjCEdit.getWrapStyleWord();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void highlight(int parIndex1, int off1, int parIndex2, int off2) {
        try {
            bbjCEdit.highlight(parIndex1, off1, parIndex2, off2);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isEditable() {
        try {
            return bbjCEdit.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void removeAll() {
        try {
            bbjCEdit.removeAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeParagraph(int parIndex) {
        try {
            bbjCEdit.removeParagraph(parIndex);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public MultilineEdit setEditable(boolean editable) {
        try {
            bbjCEdit.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setHorizontalScrollable(boolean scroll) {
        try {
            bbjCEdit.setHorizontalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setIgnoreEnters(boolean ignore) {
        try {
            bbjCEdit.setIgnoreEnters(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setIgnoreTabs(boolean ignore) {
        try {
            bbjCEdit.setIgnoreTabs(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setLimitToOneParagraph(boolean limit) {
        try {
            bbjCEdit.setLimitToOneParagraph(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setLineCountLimit(int limit) {
        try {
            bbjCEdit.setLineCountLimit(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setLineWrap(boolean wrap) {
        try {
            bbjCEdit.setLineWrap(wrap);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setMaxParagraphSize(int size) {
        try {
            bbjCEdit.setMaxParagraphSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setMaxLength(int length) {
        try {
            bbjCEdit.setMaxLength(length);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setOvertypeMode(boolean overtype) {
        try {
            bbjCEdit.setOvertypeMode(overtype);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setTabSize(int size) {
        try {
            bbjCEdit.setTabSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setVerticalScrollable(boolean scroll) {
        try {
            bbjCEdit.setVerticalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public MultilineEdit setWrapStyleWord(boolean word) {
        try {
            bbjCEdit.setWrapStyleWord(word);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public IExpansible setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
