DECLARE

  -- "lt_param" represents memeber function/procedure parameter
  TYPE lt_param IS RECORD(
    param_name   VARCHAR2(1000), -- parameter name lowercased
    param_type   VARCHAR2(1000), -- parameter type name (e.g.: 'NUMBER', 'DATE', 'BOOLEAN', '{schema}.{UDT}', etc.). In case of SELF parameter this field will be empty
    param_in_out VARCHAR2(1000) -- parameter mode. Available values: 'IN', 'OUT', 'IN OUT'
    );
  -- "lt_params" represents list of parameters indexed from first parameters position to the last
  TYPE lt_params IS TABLE OF lt_param INDEX BY PLS_INTEGER;
  -- "lt_method" represents a method of Objet Type that can be overriden
  TYPE lt_method IS RECORD(
    method_name VARCHAR2(1000), -- the name of method that can be overriden in UPPERCASE
    method_type VARCHAR2(1000), -- the type of method. Available values: 'PROCEDURE' or 'FUNCTION'
    params      lt_params, -- list of parameters indexed from first parameters position to the last
    return_type VARCHAR2(1000) -- the name of returning type in case of function (e.g.: 'NUMBER', 'DATE', 'BOOLEAN', '{schema}.{UDT}', etc.)
    );
  -- "lt_params" represents a list of methods that can be overriden
  TYPE lt_methods IS TABLE OF lt_method INDEX BY PLS_INTEGER;

  lv_methods lt_methods;
  ll_res     CLOB;

  /** This function returns a list of all methods (member procedures/functions) that CAN be overriden
   * @param par_supertype_owner      The owner of supertype that must be extended
   * @param par_supertype_name       The name of supertype that must be extended
   * @return                         List of methods that can be overriden
  */
  FUNCTION PREPARE_METHODS
  (
    par_supertype_owner IN VARCHAR2,
    par_supertype_name  IN VARCHAR2
  ) RETURN lt_methods IS
  
    -- hash of unique Objet Type procedure/function that includes: method name, all parameters (with name, type and mode) and return type (in case of function) hash
    SUBTYPE lt_hash IS VARCHAR2(1000);
  
    TYPE lt_final_methods IS TABLE OF BOOLEAN INDEX BY lt_hash;
    TYPE lt_added_methods IS TABLE OF BOOLEAN INDEX BY lt_hash;
  
    lv_methods       lt_methods;
    lv_method        lt_method;
    lv_final_methods lt_final_methods;
    lv_added_methods lt_added_methods;
    lc_hash          lt_hash;
  
    CURSOR c_methods
    (
      p_type_owner VARCHAR2,
      p_type_name  VARCHAR2
    ) IS
      SELECT *
      FROM   (SELECT m.*,
                     (SELECT DECODE(SUM(CASE
                                          WHEN a.ARGUMENT_NAME = 'SELF' THEN
                                           1
                                          ELSE
                                           0
                                        END),
                                    0,
                                    'YES',
                                    'NO') /* only STATIC functions/procedures does not have parameter with 'SELF' name */
                      FROM   all_arguments a
                      WHERE  a.OWNER = m.owner
                      AND    a.PACKAGE_NAME = m.type_name
                      AND    a.OBJECT_NAME = m.METHOD_NAME
                      AND    a.SUBPROGRAM_ID = m.SUBPROGRAM_ID
                      AND    a.DATA_LEVEL = 0) is_static
              FROM   (SELECT m.*,
                             t.lvl,
                             row_number() OVER(PARTITION BY m.OWNER, m.TYPE_NAME ORDER BY m.METHOD_NO) subprogram_id
                      FROM   all_type_methods m,
                             (SELECT TYPE_NAME,
                                     OWNER,
                                     LEVEL lvl
                              FROM   all_types t
                              START  WITH t.owner = UPPER(p_type_owner)
                                   AND    t.type_name = UPPER(p_type_name)
                              CONNECT BY PRIOR supertype_owner = t.owner
                                  AND    PRIOR supertype_name = t.type_name) t
                      WHERE  m.owner = t.owner
                      AND    m.type_name = t.type_name
                            /* skip inherited methods to properly generate 'subprogram_id' field by which we can uniquely identify record in 'all_arguments' view */
                      AND    m.inherited = 'NO')
                     
                     m)
      WHERE  is_static = 'NO'
            /* skip constructor functions */
      AND    type_name <> method_name
            /* skip MAP and ORDER functions */
      AND    method_type = 'PUBLIC'
      ORDER  BY lvl,
                method_no;
    r_methods c_methods%ROWTYPE;
    ----
    PROCEDURE PREPARE_METHOD
    (
      par_meta   IN c_methods%ROWTYPE,
      par_hash   OUT VARCHAR2,
      par_method OUT lt_method
    ) IS
    
      lc_return_type VARCHAR2(1000);
      lv_param       lt_param;
      lv_params      lt_params;
    
      CURSOR c_args IS
        SELECT a.ARGUMENT_NAME,
               DECODE(a.DATA_TYPE, 'PL/SQL BOOLEAN', 'BOOLEAN', a.DATA_TYPE) DATA_TYPE,
               a.DEFAULTED,
               REPLACE(a.in_out, '/', ' ') in_out,
               a.type_owner,
               a.type_name
        FROM   all_arguments a
        WHERE  a.OWNER = par_meta.owner
        AND    a.PACKAGE_NAME = par_meta.type_name
        AND    a.OBJECT_NAME = par_meta.method_name
        AND    a.SUBPROGRAM_ID = par_meta.subprogram_id
        AND    (a.ARGUMENT_NAME IS NOT NULL OR a.POSITION = 0)
        ORDER  BY a.SEQUENCE;
      r_args c_args%ROWTYPE;
      ----
      PROCEDURE ADD_TO_HASH(par_str IN VARCHAR2) IS
      BEGIN
        IF par_str IS NOT NULL
        THEN
          par_hash := dbms_crypto.Hash(utl_raw.cast_to_raw(par_hash || par_str), dbms_crypto.HASH_SH1);
        END IF;
      END ADD_TO_HASH;
      ----
      FUNCTION PREPARE_PARAM_TYPE(par_args IN c_args%ROWTYPE) RETURN VARCHAR2 IS
      BEGIN
        IF UPPER(par_args.argument_name) = 'SELF'
        THEN
          RETURN(NULL);
        ELSIF par_args.type_owner IS NOT NULL AND
              par_args.type_name IS NOT NULL
        THEN
          RETURN(LOWER(par_args.type_owner || '.' || par_args.type_name));
        ELSE
          RETURN(LOWER(par_args.data_type));
        END IF;
      END PREPARE_PARAM_TYPE;
      ----
    BEGIN
    
      FOR r_args IN c_args()
      LOOP
      
        IF r_args.argument_name IS NULL
        THEN
          lc_return_type := PREPARE_PARAM_TYPE(r_args);
        
          ADD_TO_HASH(lc_return_type);
        ELSE
          lv_param.param_type := PREPARE_PARAM_TYPE(r_args);
          lv_param.param_name := LOWER(r_args.argument_name);
          lv_param.param_in_out := r_args.in_out;
          lv_params(NVL(lv_params.count(), 0) + 1) := lv_param;
        
          ADD_TO_HASH(lv_param.param_type);
          ADD_TO_HASH(lv_param.param_name);
          ADD_TO_HASH(lv_param.param_in_out);
        
        END IF;
      
      END LOOP;
    
      par_method.method_name := UPPER(par_meta.method_name);
      par_method.method_type := CASE
                                  WHEN lc_return_type IS NULL THEN
                                   'PROCEDURE'
                                  ELSE
                                   'FUNCTION'
                                END;
      par_method.params := lv_params;
      par_method.return_type := lc_return_type;
    
      ADD_TO_HASH(par_method.method_name || par_method.method_type);
    
    END PREPARE_METHOD;
    ----
  BEGIN
  
    FOR r_methods IN c_methods(par_supertype_owner, par_supertype_name)
    LOOP
    
      PREPARE_METHOD(r_methods, lc_hash, lv_method);
    
      IF r_methods.final = 'YES'
      THEN
        lv_final_methods(lc_hash) := TRUE;
      END IF;
    
      IF lc_hash IS NOT NULL AND
         NOT lv_added_methods.exists(lc_hash) AND
         NOT lv_final_methods.exists(lc_hash)
      THEN
        lv_added_methods(lc_hash) := TRUE;
        lv_methods(NVL(lv_methods.count(), 0) + 1) := lv_method;
      END IF;
    
    END LOOP;
  
    RETURN(lv_methods);
  END PREPARE_METHODS;
  ----
  PROCEDURE CREATE_NEW_TYPE_SCRIPT
  (
    par_res             IN OUT NOCOPY CLOB,
    par_supertype_owner IN VARCHAR2,
    par_supertype_name  IN VARCHAR2,
    par_methods         IN lt_methods
  ) IS
  
    lc_new_type VARCHAR2(1000);
  
    ----
    PROCEDURE WA(par_str IN VARCHAR2) IS
    BEGIN
      IF par_str IS NOT NULL
      THEN
        DBMS_LOB.writeappend(par_res, LENGTH(par_str), par_str);
      END IF;
    END WA;
    ----
    PROCEDURE WL(par_str IN VARCHAR2) IS
    BEGIN
      WA(CHR(10) || par_str);
    END WL;
    ----
    PROCEDURE ADD_COMMENTS(par_method IN lt_method) IS
    BEGIN
    
      WL('/** MethodDescription');
    
      IF par_method.params.count() > 0
      THEN
        FOR i IN 1 .. par_method.params.count()
        LOOP
          IF UPPER(par_method.params(i).param_name) <> 'SELF'
          THEN
            WL(' * @param  ' || RPAD(par_method.params(i).param_name, 31, ' ') || ' ParameterDescription');
          END IF;
        END LOOP;
      END IF;
    
      IF par_method.return_type IS NOT NULL
      THEN
        WL(' * @return ' || RPAD(' ', 31, ' ') || ' ParameterDescription');
      END IF;
    
      WL('*/');
    
    END ADD_COMMENTS;
    ----
    PROCEDURE ADD_METHOD_SPEC(par_method IN lt_method) IS
    BEGIN
      WL(' OVERRIDING MEMBER ' || par_method.method_type || ' ' || par_method.method_name);
    
      IF par_method.params.count() > 0
      THEN
        WL('(');
        FOR i IN 1 .. par_method.params.count()
        LOOP
          IF i > 1
          THEN
            WL(',');
          END IF;
        
          IF UPPER(par_method.params(i).param_name) = 'SELF'
          THEN
            WL(par_method.params(i).param_name || ' ' || par_method.params(i).param_in_out || ' ' || LOWER(lc_new_type));
          ELSE
            WL(par_method.params(i).param_name || ' ' || par_method.params(i).param_in_out || ' ' || par_method.params(i).param_type);
          END IF;
        
        END LOOP;
        WL(')');
      END IF;
    
      IF par_method.return_type IS NOT NULL
      THEN
        WL(' RETURN ' || par_method.return_type);
      END IF;
    
    END ADD_METHOD_SPEC;
    ----
    PROCEDURE APPEND_TYPE_SPEC IS
    BEGIN
      WA('CREATE OR REPLACE TYPE ' || LOWER(USER) || '.' || lc_new_type || ' FORCE UNDER ' ||
         LOWER(par_supertype_owner) || '.' || LOWER(par_supertype_name) || '(');
      WL('/**');
      WL(' *  ObjectDescription');
      WL('*/');
      WL(' ');
    
      WL('  --------------------------------------------------------------------------------------------------');
      WL(' /** Empty constructor */');
      WL(' CONSTRUCTOR FUNCTION ' || lc_new_type || ' RETURN SELF AS RESULT');
    
      FOR i IN 1 .. NVL(par_methods.count(), 0)
      LOOP
      
        WA(',');
        WL('  --------------------------------------------------------------------------------------------------');
        ADD_COMMENTS(par_methods(i));
        ADD_METHOD_SPEC(par_methods(i));
      
      END LOOP;
    
      WL('');
      WL(')');
    END APPEND_TYPE_SPEC;
    ----
    PROCEDURE APPEND_TYPE_BODY IS
      ----
      PROCEDURE APPEND_CODE_SAMPLE(par_method IN lt_method) IS
        lb_first_param BOOLEAN;
      BEGIN
        WL('IS');
        IF par_method.return_type IS NOT NULL
        THEN
          WL('  lv_supertype_result ' || par_method.return_type || CASE
               WHEN UPPER(par_method.return_type) LIKE ('%VARCHAR%') THEN
                '(32767)'
               ELSE
                NULL
             END || ';');
        END IF;
        WL('BEGIN');
        WL('/* -- uncomment this code if you want to inherit supertype logic');
        IF par_method.return_type IS NOT NULL
        THEN
          WL('lv_supertype_result := ');
        END IF;
        WL('(SELF AS ' || LOWER(par_supertype_owner || '.' || par_supertype_name) || ').' ||
           UPPER(par_method.method_name) || '(');
        FOR i IN 1 .. NVL(par_method.params.count(), 0)
        LOOP
          lb_first_param := TRUE;
          IF UPPER(par_method.params(i).param_name) <> 'SELF'
          THEN
            IF NOT lb_first_param
            THEN
              WA(',');
            END IF;
            WL(LOWER(par_method.params(i).param_name) || ' => ' || LOWER(par_method.params(i).param_name));
            lb_first_param := FALSE;
          END IF;
        END LOOP;
        WL(');');
        WL('*/');
        WL('');
        WL('-- implement subtype logic here');
        IF par_method.return_type IS NULL
        THEN
          WL('NULL;');
        ELSE
          WL('RETURN(NULL);');
        END IF;
        WL('END ' || par_method.method_name || ';');
      END APPEND_CODE_SAMPLE;
      ----
    BEGIN
      WL('CREATE OR REPLACE TYPE BODY ' || LOWER(USER) || '.' || lc_new_type || ' IS');
      WL('');
    
      WL('  --------------------------------------------------------------------------------------------------');
      WL(' CONSTRUCTOR FUNCTION ' || lc_new_type || ' RETURN SELF AS RESULT IS');
      WL(' BEGIN');
      WL(' RETURN;');
      WL(' END ' || lc_new_type || ';');
    
      FOR i IN 1 .. NVL(par_methods.count(), 0)
      LOOP
      
        WL('--------------------------------------------------------------------------------------------------');
        ADD_METHOD_SPEC(par_methods(i));
        APPEND_CODE_SAMPLE(par_methods(i));
      
      END LOOP;
      WL('  --------------------------------------------------------------------------------------------------');
    
      WL('END;');
    END APPEND_TYPE_BODY;
    ----
  BEGIN
  
    lc_new_type := LOWER(par_supertype_name) || '_sub';
  
    APPEND_TYPE_SPEC();
    WL('/');
    APPEND_TYPE_BODY();
    WL('/');
    WL('');
  
  END CREATE_NEW_TYPE_SCRIPT;
  ----
  PROCEDURE PRINT_RESULT_TO_DBMS_OUTPUT(par_clob IN OUT NOCOPY CLOB) IS
    ln_offset NUMBER := 1;
    ln_amount NUMBER;
    ln_length NUMBER := DBMS_LOB.getLength(par_clob);
    lc_buffer VARCHAR2(32767);
  BEGIN
    DBMS_OUTPUT.ENABLE(10000000);
    WHILE ln_offset < ln_length
    LOOP
      ln_amount := DBMS_LOB.INSTR(par_clob, CHR(10), ln_offset) - ln_offset;
      IF ln_amount > 0
      THEN
        DBMS_LOB.read(par_clob, ln_amount, ln_offset, lc_buffer);
        ln_offset := ln_offset + ln_amount + 1;
      ELSE
        lc_buffer := NULL;
        ln_offset := ln_offset + 1;
      END IF;
      DBMS_OUTPUT.PUT_LINE(lc_buffer);
    END LOOP;
  END PRINT_RESULT_TO_DBMS_OUTPUT;
  ----
BEGIN

  lv_methods := PREPARE_METHODS(:type_owner, :type_name);

  DBMS_LOB.createtemporary(ll_res, FALSE);
  CREATE_NEW_TYPE_SCRIPT(ll_res, :type_owner, :type_name, lv_methods);

  PRINT_RESULT_TO_DBMS_OUTPUT(ll_res);

  DBMS_LOB.FREETEMPORARY(ll_res);

END;
