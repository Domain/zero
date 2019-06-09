module zero.system.event;

import std.traits : isDelegate, isFunction, isCallable, ParameterTypeTuple, ReturnType;
import std.functional : toDelegate;

struct Event(Prototype) if (isCallable!Prototype)
{
    static if (isDelegate!Prototype || isFunction!Prototype)
        public alias DelegateType = Prototype;
    else
        public alias DelegateType = typeof(&Prototype);

    private DelegateType[] observers;
    private DelegateType[] toDel;
    private DelegateType[] toAdd;
    private int callingCount = 0;

    void opOpAssign(string op)(DelegateType observer) 
        if (op == "+" || op == "~" || op == "-")
    {
        static if (op != "-")
        {
            if (callingCount == 0)
                observers ~= observer;
            else
                toAdd ~= observer;
        }
        else
        {
            import std.algorithm : countUntil, remove;
            auto i = observers.countUntil(observer);
            if (i != -1)
            {
                if (callingCount == 0)
                    observers = observers.remove(i);
                else
                    toDel ~= observer;
            }
        }
    }

    void opOpAssign(string op, C)(C observer) 
        if ((op == "+" || op == "~" || op == "-") && isCallable!C && !isDelegate!C)
    {
        mixin("this " ~ op ~ "= toDelegate(observer);");
    }

    private enum cleanup = `
        callingCount++;
        scope (exit) 
        {
            callingCount--;
            if (callingCount == 0)
            {
                foreach (ob; toDel)
                {
                    this -= ob;
                }

                foreach (ob; toAdd)
                {
                    this ~= ob;
                }
            }
        }
        `;

    ReturnType!DelegateType opCall(ParameterTypeTuple!DelegateType args)
    {
        mixin(cleanup);

        static if (is(ReturnType!DelegateType == void))
        {
            foreach (ob; observers)
            {
                ob(args);
            }
        }
        else
        {
            ReturnType!DelegateType result;
            foreach (ob; observers)
            {
                result = ob(args);
            }
            return result;
        }
    }

    static if (!is(ReturnType!DelegateType == void))
    {
        bool callUntil(ReturnType!DelegateType result, ParameterTypeTuple!DelegateType args)
        {
            mixin(cleanup);

            foreach (ob; observers)
            {
                if (ob(args) == result)
                    return true;
            }
            return false;
        }
    }
}
