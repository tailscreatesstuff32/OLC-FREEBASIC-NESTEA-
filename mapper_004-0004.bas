
'$IF IUNCLUDED = UNDEFINED THEN


REM    '$include: 'mapper_004.bi'
'$END IF




REM '$include: 'mapper.bm'



SUB mapper_004 (prg_banks AS uint8_t, chr_banks AS uint8_t)


   ' REDIM vRAMStatic(32 * 1024)
    Mapper prg_banks, chr_banks,4


END SUB

FUNCTION cpuMap04Read (addr AS uint16_t, mapped_addr AS uint32_t, byte_data AS uint8_t)
   ' mapped_addr = 0
    'IF addr >= &H6000~% AND addr <= &H7FFF~% THEN

    '    mapped_addr = &HFFFFFFFF~&

    '    byte_data = vRAMStatic(addr AND &H1FFF~%)


    '    cpuMap04Read = 1
    '   EXIT FUNCTION
    'END IF



    IF addr >= &H8000 AND addr <= &H9FFF THEN

        mapped_addr = pPRGBank(0) + (addr AND &H1FFF)

        cpuMap04Read = 1
       EXIT FUNCTION
    END IF

    IF addr >= &HA000 AND addr <= &HBFFF THEN

        mapped_addr = pPRGBank(1) + (addr AND &H1FFF~%)

        cpuMap04Read = 1
       EXIT FUNCTION
    END IF

    IF addr >= &HC000~% AND addr <= &HDFFF~% THEN

        mapped_addr = pPRGBank(2) + (addr AND &H1FFF~%)

        cpuMap04Read = 1
       EXIT FUNCTION
    END IF

    IF addr >= &HE000~% AND addr <= &HFFFF~% THEN

        mapped_addr = pPRGBank(3) + (addr AND &H1FFF~%)

        cpuMap04Read = 1
        EXIT FUNCTION
    END IF




    cpuMap04Read = 0

END FUNCTION


FUNCTION cpuMap04Write (addr AS _UNSIGNED INTEGER, mapped_addr AS _UNSIGNED LONG, byte_data AS _UNSIGNED _BYTE)
     '   dim temp_addr as   _UNSIGNED INTEGER
  '  mapped_addr = 0

    'IF addr >= &H6000~% AND addr <= &H7FFF~% THEN

    '    mapped_addr = &HFFFFFFFF~&

    '    vRAMStatic(addr AND &H1FFF~%) = byte_data


    '    cpuMap04Write = 1
    ' EXIT FUNCTION
    'END IF


    IF (addr >= &H8000~% AND addr <= &H9FFF~%) THEN


   '    temp_addr = addr and &H0001

        ' Bank Select
      IF (addr AND &H0001) = 0 THEN

            nTargetRegister = (byte_data AND &H07)
            bPRGBankMode = (byte_data AND &H40)
            bCHRInversion = (byte_data AND &H80)

              ' _title hex$(addr)
        ELSE
           pRegister(nTargetRegister) = byte_data
            '// Update Pointer Table
            IF (bCHRInversion) THEN

                pCHRBank(0) = pRegister(2) * &H0400~%
                pCHRBank(1) = pRegister(3) * &H0400~%
                pCHRBank(2) = pRegister(4) * &H0400~%
                pCHRBank(3) = pRegister(5) * &H0400~%
                pCHRBank(4) = (pRegister(0) AND &HFE~%) * &H0400~%
                pCHRBank(5) = pRegister(0) * &H0400~% + &H0400~%
                pCHRBank(6) = (pRegister(1) AND &HFE~%) * &H0400~%
                pCHRBank(7) = pRegister(1) * &H0400~% + &H0400~%

            ELSE

                pCHRBank(0) = (pRegister(0) AND &HFE~%) * &H0400~%
                pCHRBank(1) = pRegister(0) * &H0400~% + &H0400~%
                pCHRBank(2) = (pRegister(1) AND &HFE~%) * &H0400~%
                pCHRBank(3) = pRegister(1) * &H0400~% + &H0400~%
                pCHRBank(4) = pRegister(2) * &H0400~%
                pCHRBank(5) = pRegister(3) * &H0400~%
                pCHRBank(6) = pRegister(4) * &H0400~%
                pCHRBank(7) = pRegister(5) * &H0400~%
            END IF

            IF (bPRGBankMode) THEN

                    pPRGBank(2) = (pRegister(6) AND &H3F~%) * &H2000~%
                    pPRGBank(0) = (n_PRGbanks * 2 - 2) * &H2000~%

            ELSE

                   pPRGBank(0) = (pRegister(6) AND &H3F~%) * &H2000~%
                   pPRGBank(2) = (n_PRGbanks * 2 - 2) * &H2000~%
            END IF

              pPRGBank(1) = (pRegister(7) AND &H3F~%) * &H2000~%
              pPRGBank(3) = (n_PRGbanks * 2 - 1) * &H2000~%


        END IF

            cpuMap04Write = 0
           EXIT FUNCTION

    END IF
    IF (addr >= &HA000~% AND addr <= &HBFFF~%) THEN

        IF ((addr AND &H0001)) = 0 THEN

        '    '// Mirroring
        '    IF (byte_data AND &H01) THEN
        '        mirrormode = horizontal
        '    ELSE
        '        mirrormode = vertical

        '    END IF
        'else

            ' PRG Ram Protect
            ' TODO:

        END IF
            cpuMap04Write = 0
          EXIT FUNCTION


    END IF

    IF (addr >= &HC000~% AND addr <= &HDFFF~%) THEN

        IF ((addr AND &H0001))= 0 THEN

            nIRQReload = byte_data

        ELSE

            nIRQCounter = &H0000

        END IF
            cpuMap04Write = 0
         EXIT FUNCTION

    END IF

    IF (addr >= &HE000~% AND addr <= &HFFFF~%) THEN

        IF ((addr AND &H0001))= 0 THEN

            bIRQEnable = 0
            bIRQActive = 0

        ELSE

            bIRQEnable = 1

        END IF
        cpuMap04Write = 0
       EXIT FUNCTION


    END IF

END FUNCTION


FUNCTION ppuMap04Read (addr AS _UNSIGNED INTEGER, mapped_addr AS _UNSIGNED LONG)

    'IF  addr < &H800~% THEN
    '    mapped_addr = _shr(224,1) * &H800~& + (addr and &H7ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION

    ''IF  addr < &H800~% THEN
    ''    mapped_addr = _shr(0,1) * &H800~& + (addr and &H7ff)
    ''    ppuMap04Read = 1
    ''    EXIT FUNCTION

    'elseIF addr < &H1000~% THEN
    '    mapped_addr = _shr(226,1)* &H800~& + (addr  and &H7ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION

    'elseIF addr < &H1400~% THEN
    '    mapped_addr = 2* &H400~& + (addr  and &H3ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseIF addr < &H1400~% THEN
    '    mapped_addr = 31* &H400~& + (addr  and &H3ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION


    'elseIF addr < &H1800~% THEN
    '    mapped_addr = 31* &H400~& + (addr  and &H3ff~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseIF addr < &H1C00~% THEN
    '    mapped_addr = 31* &H400~& + (addr  and &H3ff~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseif   addr < &H1FFF~%  then
    '   mapped_addr = 31 * &H400~& + (addr and &H3FF~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'END IF







    'IF  addr < &H800~% THEN
    '    mapped_addr = _shr(0,1) * &H800~& + (addr and &H7ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION



    'IF  addr < &H800~% THEN
    '    mapped_addr = _shr(pCHRBank(0),1) * &H800~& + (addr and &H7ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION


    'elseIF addr < &H1000~% THEN
    '    mapped_addr = _shr(pCHRBank(1),1)* &H800~& + (addr  and &H7ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION

    'elseIF addr < &H1400~% THEN
    '    mapped_addr = pCHRBank(2)* &H400~& + (addr  and &H3ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseIF addr < &H1400~% THEN
    '    mapped_addr = pCHRBank(3)* &H400~& + (addr  and &H3ff)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION


    'elseIF addr < &H1800~% THEN
    '    mapped_addr = pCHRBank(3)* &H400~& + (addr  and &H3ff~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseIF addr < &H1C00~% THEN
    '    mapped_addr = 31* pCHRBank(4)+ (addr  and &H3ff~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'elseif   addr < &H1FFF~%  then
    '   mapped_addr = 31 * pCHRBank(5) + (addr and &H3FF~%)
    '    ppuMap04Read = 1
    '    EXIT FUNCTION
    'END IF



    IF addr >= &H0000~% AND addr <= &H03FF~% THEN
        mapped_addr = pCHRBank(0) + (addr AND &H03FF~&)
        ppuMap04Read = 1
     EXIT FUNCTION
    END IF

    IF addr >= &H0400~% AND addr <= &H07FF~% THEN
        mapped_addr = pCHRBank(1) + (addr AND &H03FF~&)
        ppuMap04Read = 1
      EXIT FUNCTION
    END IF

    IF addr >= &H0800~% AND addr <= &H0BFF~% THEN
        mapped_addr = pCHRBank(2) + (addr AND &H3FF~&)
        ppuMap04Read = 1
       EXIT FUNCTION
    END IF

    IF addr >= &H0C00~% AND addr <= &H0FFF~% THEN
        mapped_addr = pCHRBank(3) + (addr AND &H3FF~&)
        ppuMap04Read = 1
     EXIT FUNCTION
    END IF

    IF addr >= &H1000~% AND addr <= &H13FF~% THEN
        mapped_addr = pCHRBank(4) + (addr AND &H3FF~&)
        ppuMap04Read = 1
        EXIT FUNCTION
    END IF

    IF addr >= &H1400~% AND addr <= &H17FF~% THEN
        mapped_addr = pCHRBank(5) + (addr AND &H3FF~&)
        ppuMap04Read = 1
       EXIT FUNCTION
    END IF

    IF addr >= &H1800~% AND addr <= &H1BFF~% THEN
        mapped_addr = pCHRBank(6) + (addr AND &H3FF~&)
        ppuMap04Read = 1
        EXIT FUNCTION
    END IF
    IF addr >= &H1C00~% AND addr <= &H1FFF~% THEN
        mapped_addr = pCHRBank(7) + (addr AND &H3FF~&)
        ppuMap04Read = 1
        EXIT FUNCTION
    END IF

END FUNCTION


FUNCTION ppuMap04Write (addr AS _UNSIGNED INTEGER, mapped_addr AS _UNSIGNED LONG)

    ppuMap04Write = 0

END FUNCTION

SUB Mapper_004_Reset ()

    nTargetRegister = 0
    bPRGBankMode = 0
    bCHRInversion = 0
    mirrormode = 0

    bIRQActive = 0
    bIRQEnable = 0
    bIRQUpdate = 0
    nIRQCounter = 0
    nIRQReload = 0

       'for i = 0 to 4-1
       '     pPRGBank(i) = 0
       'next i

       'for i = 0 to 8-1
       '     pCHRBank(i) = 0
       '     pRegister(i) = 0
       'next i


    pPRGBank(0) = 0 * &H2000~%
    pPRGBank(1) = 1 * &H2000~%
    pPRGBank(2) = (8 * 2 - 2) * &H2000~%
    pPRGBank(3) = (8 * 2 - 1) * &H2000~%







END SUB
