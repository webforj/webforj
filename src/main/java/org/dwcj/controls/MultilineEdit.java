package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;

public final class MultilineEdit extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjCEdit multilineEdit;

    public MultilineEdit() {
    }

    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText());
            catchUp();
            multilineEdit = (BBjCEdit) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void addParagraph(int index, String paragraph) {
        try {
            multilineEdit.addParagraph(index, paragraph);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void addParagraphs(int index, ArrayList<String> paragraphs) {
        try {
            multilineEdit.addParagraphs(index, (BBjVector) paragraphs);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void appendToParagraph(int parNum, String text) {
        try {
            multilineEdit.appendToParagraph(parNum, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public ArrayList<String> getAllParagraphs() {
        try {
            return (ArrayList<String>) multilineEdit.getAllParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public int getCurrentParagraphIndex() {
        try {
            return multilineEdit.getCurrentParagraphIndex();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getHorizontalScrollable() {
        try {
            return multilineEdit.getHorizontalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getIgnoreEnters() {
        try {
            return multilineEdit.getIgnoreEnters();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getIgnoreTabs() {
        try {
            return multilineEdit.getIgnoreTabs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getLimitToOneParagraph() {
        try {
            return multilineEdit.getLimitToOneParagraph();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getLineCountLimit() {
        try {
            return multilineEdit.getLineCountLimit();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getLineWrap() {
        try {
            return multilineEdit.getLineWrap();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getMaxParagraphSize() {
        try {
            return multilineEdit.getMaxParagraphSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMaxLength() {
        return multilineEdit.getMaxLength();
    }

    public int getNumberOfParagraphs() {
        try {
            return multilineEdit.getNumberOfParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getOvertypeMode() {
        try {
            return multilineEdit.getOvertypeMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getParagraph(int parNum) {
        try {
            return multilineEdit.getParagraph(parNum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public ArrayList<String> getSelection() {
        try {
            return (ArrayList<String>) multilineEdit.getSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public int getTabSize() {
        try {
            return multilineEdit.getTabSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getVerticalScrollable() {
        try {
            return multilineEdit.getVerticalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getWrapStyleWord() {
        try {
            return multilineEdit.getWrapStyleWord();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void highlight(int parIndex1, int off1, int parIndex2, int off2) {
        try {
            multilineEdit.highlight(parIndex1, off1, parIndex2, off2);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isEditable() {
        try {
            return multilineEdit.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void removeAll() {
        try {
            multilineEdit.removeAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeParagraph(int parIndex) {
        try {
            multilineEdit.removeParagraph(parIndex);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditable(boolean editable) {
        try {
            multilineEdit.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setHorizontalScrollable(boolean scroll) {
        try {
            multilineEdit.setHorizontalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setIgnoreEnters(boolean ignore) {
        try {
            multilineEdit.setIgnoreEnters(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setIgnoreTabs(boolean ignore) {
        try {
            multilineEdit.setIgnoreTabs(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLimitToOneParagraph(boolean limit) {
        try {
            multilineEdit.setLimitToOneParagraph(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLineCountLimit(int limit) {
        try {
            multilineEdit.setLineCountLimit(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLineWrap(boolean wrap) {
        try {
            multilineEdit.setLineWrap(wrap);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaxParagraphSize(int size) {
        try {
            multilineEdit.setMaxParagraphSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaxLength(int length) {
        try {
            multilineEdit.setMaxLength(length);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setOvertypeMode(boolean overtype) {
        try {
            multilineEdit.setOvertypeMode(overtype);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setTabSize(int size) {
        try {
            multilineEdit.setTabSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setVerticalScrollable(boolean scroll) {
        try {
            multilineEdit.setVerticalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setWrapStyleWord(boolean word) {
        try {
            multilineEdit.setWrapStyleWord(word);
        } catch (BBjException e) {
            e.printStackTrace();
        }
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
