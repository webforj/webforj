package org.dwcj.controls;


public interface IExpansible {

    void setExpanse(Expanse expanse);

    enum Expanse {
        LARGE,
        MEDIUM,
        SMALL,
        XLARGE,
        XSMALL
    }

}
