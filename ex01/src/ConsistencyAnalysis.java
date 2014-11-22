import java.util.HashMap;
import java.util.Map;

import soot.Local;
import soot.Unit;
import soot.jimple.BinopExpr;
import soot.jimple.DefinitionStmt;
import soot.jimple.EqExpr;
import soot.jimple.GeExpr;
import soot.jimple.GtExpr;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.IntConstant;
import soot.jimple.LeExpr;
import soot.jimple.LtExpr;
import soot.jimple.NeExpr;
import soot.jimple.ReturnStmt;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.scalar.BackwardFlowAnalysis;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.FuncDecl;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.Sort;
import com.microsoft.z3.Z3Exception;


public class ConsistencyAnalysis extends BackwardFlowAnalysis<Unit, AnalysisContainer> {

    Context ctx;
    Map<String, IntExpr> m1;
    FuncDecl Field;

    public ConsistencyAnalysis(DirectedGraph<Unit> graph) {
        super(graph);

        HashMap<String, String> cfg = new HashMap<String, String>();
        cfg.put("model", "true");
        try {
            ctx = new Context(cfg);
            Field = ctx.MkFuncDecl("Field", new Sort[] {ctx.IntSort(), ctx.IntSort()}, ctx.IntSort());
        } catch (Z3Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        m1 = new HashMap<String, IntExpr>();
        //set up buncha data structures

        doAnalysis();
    }

    @Override
        protected void flowThrough(AnalysisContainer arg0, Unit arg1, AnalysisContainer arg2) {
        // TODO Auto-generated method stub
        try {
            if (arg1 instanceof ReturnStmt) {
                String name = ((ReturnStmt)arg1).getOp().toString();
                IntExpr temp1 = ctx.MkIntConst(name);
                m1.put(name, temp1);
                arg2.lt0 = ctx.MkLt(temp1, ctx.MkInt(0));
                arg2.eq0 = ctx.MkEq(temp1, ctx.MkInt(0));
                arg2.gt0 = ctx.MkGt(temp1, ctx.MkInt(0));

            } else if (arg1 instanceof DefinitionStmt) {
                DefinitionStmt arg11 = ((DefinitionStmt)arg1);
                Expr temp2 = m1.get(arg11.getLeftOp().toString());
                if (temp2 != null) {
                    if (arg11.getRightOp() instanceof IntConstant ||
                        arg11.getRightOp() instanceof InstanceFieldRef ||
                        arg11.getRightOp() instanceof Local) {
                        IntExpr temp1;
                        if (arg11.getRightOp() instanceof IntConstant)
                            temp1 = ctx.MkInt(((IntConstant) arg11.getRightOp()).value);
                        else if (arg11.getRightOp() instanceof InstanceFieldRef) {
                            String name3 = ((InstanceFieldRef)arg11.getRightOp()).getBase().toString();
                            IntExpr temp3 = m1.get(name3);
                            if (temp3 == null) {
                                temp3 = ctx.MkIntConst(name3);
                                m1.put(name3, temp3);
                            }
                            String name4 = ((InstanceFieldRef)arg11.getRightOp()).getField().toString();
                            IntExpr temp4 = m1.get(name4);
                            if (temp4 == null) {
                                temp4 = ctx.MkIntConst(name4);
                                m1.put(name4, temp4);
                            }
                            temp1 = (IntExpr) ctx.MkApp(Field, new Expr[] {temp3, temp4});
                        } else /*Local*/ {
                            String name = arg11.getRightOp().toString();
                            temp1 = m1.get(name);
                            if (temp1 == null) {
                                temp1 = ctx.MkIntConst(name);
                                m1.put(name, temp1);
                            }
                        }
                        Expr[] temp3 = new Expr[] {temp2}, temp4 = new Expr[] {temp1};
                        arg2.lt0 = (BoolExpr) arg0.lt0.Substitute(temp3, temp4);
                        arg2.eq0 = (BoolExpr) arg0.eq0.Substitute(temp3, temp4);
                        arg2.gt0 = (BoolExpr) arg0.gt0.Substitute(temp3, temp4);
                    } else
                        copy(arg0, arg2);
                } else
                    copy(arg0, arg2);
            } else 
                copy(arg0, arg2);
        } catch (Z3Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }			
    }

    @Override
        protected void copy(AnalysisContainer arg0, AnalysisContainer arg1) {
        arg1.lt0 = arg0.lt0;
        arg1.eq0 = arg0.eq0;
        arg1.gt0 = arg0.gt0;
    }

    @Override
        protected AnalysisContainer entryInitialFlow() {
        AnalysisContainer temp1 = new AnalysisContainer ();
        try {
            temp1.lt0 = this.ctx.MkTrue();
            temp1.eq0 = this.ctx.MkTrue();
            temp1.gt0 = this.ctx.MkTrue();
        } catch (Z3Exception e) {
            e.printStackTrace();
        } finally {
            return temp1;
        }
    }

    @Override
        protected void merge(AnalysisContainer arg0, AnalysisContainer arg1, AnalysisContainer arg2) {
        try {
            arg2.lt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.lt0, arg0.lt0});
            arg2.eq0 = this.ctx.MkAnd(new BoolExpr[] {arg1.eq0, arg0.eq0});
            arg2.gt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.gt0, arg0.gt0});
        } catch (Z3Exception e) {
            e.printStackTrace();
        }
    }

    @Override
        protected void merge(Unit arg0, AnalysisContainer arg1, AnalysisContainer arg2, AnalysisContainer arg3) {
        try {
            if (arg0 instanceof IfStmt) {
                if (((IfStmt)arg0).getCondition() instanceof BinopExpr) {
                    String name1 = ((BinopExpr)((IfStmt)arg0).getCondition()).getOp1().toString();
                    IntExpr temp1 = m1.get(name1);
                    if (temp1 == null) {
                        temp1 = ctx.MkIntConst(name1);
                        m1.put(name1, temp1);
                    }
                    String name2 = ((BinopExpr)((IfStmt)arg0).getCondition()).getOp2().toString();
                    IntExpr temp2 = m1.get(name2);
                    if (temp2 == null) {
                        temp2 = ctx.MkIntConst(name2);
                        m1.put(name2, temp2);
                    }
                    Object temp4 = ((IfStmt)arg0).getCondition();
                    BoolExpr temp3 =
                        (temp4 instanceof EqExpr) ? ctx.MkEq(temp1, temp2) :
                        (temp4 instanceof GeExpr) ? ctx.MkGe(temp1, temp2) :
                        (temp4 instanceof GtExpr) ? ctx.MkGt(temp1, temp2) :
                        (temp4 instanceof LeExpr) ? ctx.MkLe(temp1, temp2) :
                        (temp4 instanceof LtExpr) ? ctx.MkLt(temp1, temp2) :
                        (temp4 instanceof LtExpr) ? ctx.MkLt(temp1, temp2) :
                        (temp4 instanceof NeExpr) ? ctx.MkNot(ctx.MkEq(temp1, temp2)) : null;
                    BoolExpr temp5 = ctx.MkNot(temp3);
                    arg3.lt0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.lt0), this.ctx.MkImplies(temp5, arg1.lt0)});
                    arg3.eq0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.eq0), this.ctx.MkImplies(temp5, arg1.eq0)});
                    arg3.gt0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.gt0), this.ctx.MkImplies(temp5, arg1.gt0)});
                }
            } else {
                arg3.lt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.lt0, arg2.lt0});
                arg3.eq0 = this.ctx.MkAnd(new BoolExpr[] {arg1.eq0, arg2.eq0});
                arg3.gt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.gt0, arg2.gt0});
            }
        } catch (Z3Exception e) {
            e.printStackTrace();
        }
    }

    @Override
        protected AnalysisContainer newInitialFlow() {
        AnalysisContainer temp1 = new AnalysisContainer ();
        try {
            temp1.lt0 = this.ctx.MkTrue();
            temp1.eq0 = this.ctx.MkTrue();
            temp1.gt0 = this.ctx.MkTrue();
        } catch (Z3Exception e) {
            e.printStackTrace();
        } finally {
            return temp1;
        }
    }
	
}