#include <iostream>
#include <cmath>
#include <cstring>
#include <fstream>
#include <cstdlib>
#include <vector>
#include <iomanip>
#include <cfloat>
#include <limits>
#include <string>
#include <sstream>
#include <algorithm>
#include <cctype>



using namespace std;


void decal(double MJD,double MJDDEP,int HE,int MI, vector<int>& SCHH, vector<int>& SCHMIN,int i){
    int T0,TT;

    T0 = (HE*60) + MI;
    TT = T0 - (4*(MJD-MJDDEP));
    while(TT<0){
        TT = TT + 1436;
           };
    SCHH[i] = (TT/60);
    SCHMIN[i] = (TT%60);


};


//Organiza o vetor com os MJD
void classement(vector<int> (&SCHH), vector<int> (&SCHMIN), vector<long double> (&SCHTIME), int NN){
    vector<double> PROVT;
    vector<int> PROVH;
    int NDEP, i;
    vector<int>PROVM;

    PROVH.resize(NN);
    PROVM.resize(NN);
    PROVT.resize(NN);

    NDEP = 0;
    for(i=1;i<NN;i++){
        if(SCHTIME[i] > SCHTIME[NDEP]){
            NDEP = i;
        };
    };
    for(i=0;i<(NN-NDEP+1);i++){
        PROVT.at(i) = SCHTIME[NDEP+i+1];
        PROVH.at(i) = SCHH[NDEP+i+1];
        PROVM.at(i) = SCHMIN[NDEP+i+1];

    };

    for(i=(NN-NDEP-1);i<NN; i++){
        PROVT.at(i)= SCHTIME[i-(NN-NDEP-1)];
        PROVH.at(i) = SCHH[i-(NN-NDEP-1)];
        PROVM.at(i) = SCHMIN[i-(NN-NDEP-1)];
    };

    for(i=0;i<NN;i++){
        SCHTIME.at(i) = PROVT[i];
        SCHH.at(i) = PROVH[i];
        SCHMIN.at(i) = PROVM[i] ;
    };
};




void datemjd(int YN, int MN, int DN, int HN, int MINN, long double SECN, long double &TIMENAV, int k, char CODE){

    int I1 = 0, I2 = 0;
    long double DFJ = 0, D1 = 0, INT1 = 0, INT2 = 0, B = 0, A = 0;

    DFJ = (HN + (MINN/60.0) + (SECN/3600))/24;


    I1 = YN;

    if(MN < 3){
        I1 = YN - 1;
    };

    I2 = MN + 1;


    if(MN < 3){
        I2 = MN + 13;
    };

    D1 =  DN + DFJ;


    if(I1 >= 0){
        if((YN - 1582) < 0){
            A = (365.25 * I1);
            B = modf(A , &INT1);
            A = 30.6001 * I2;
            B = modf(A , &INT2);
            if((k==0)&&(CODE == 'C')){
                TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 - (14.0/86400);

            }
            else{
                TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5;   //para GPS e BEIDOU obs
            }
         }

        else if((YN - 1582) == 0){
            if((DN - 10) < 0){

                A = 365.25 * I1;
                B = modf(A , &INT1);
                A = 30.6001 * I2;
                B = modf(A , &INT2);
                if((k==0)&&(CODE == 'C')){
                    TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 - (14.0/86400);

                }
                else{
                    TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5;  //para GPS
                }

            }
            else if((DN - 10) == 0){
                if(DN < 15){

                    A = (365.25 * I1);
                    B = modf(A , &INT1);
                    A = 30.6001 * I2;
                    B = modf(A , &INT2);
                    if((k==0)&&(CODE == 'C')){
                        TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 - (14.0/86400);

                    }
                    else{
                            TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5;
                        };

                 };
            }
            else{
                A = (365.25 * I1);
                B = modf(A , &INT1);
                A = 30.6001 * I2;
                B = modf(A , &INT2);
                if((k==0)&&(CODE == 'C')){
                    TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 + (2 - (I1/100) + (I1/400)) - (14.0/86400);

                }
                else{
                    TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 + (2 - (I1/100) + (I1/400));
                };

            };
        }
        else{
            A = (365.25 * I1);
            B = modf(A , &INT1);
            A = 30.6001 * I2;
            B = modf(A , &INT2);
            if((k==0)&&(CODE == 'C')){
                TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 + (2 - (I1/100) + (I1/400)) - (14.0/86400);

            }
            else{
                TIMENAV =  INT1 + INT2 + D1 + 1720994.5 - 2400000.5 + (2 - (I1/100) + (I1/400));
            };
        };
   };

};


void leapscorr (int &DD, int &HH, int &MIN, long double &SEC, int LEAPSEC){

     SEC = SEC + LEAPSEC;

    if(SEC >= 60){
        MIN = MIN + 1;
        SEC = fmod(SEC, 60);
    };

    if(MIN  >= 60){
        HH = HH + 1;
        MIN = MIN % 60;
    };
    if(HH >= 24){
        DD = DD + 1;
        HH = HH % 24;
    };
}



void header(double SVERSION,int ITESTC1, char lengC1P1[], char dateC1P1[], string REVDATE, char FLOUT[], string RCVR, int CH, string LAB, long double ANTPOS[], string COMMENTS, long double RECDELP1, long double RECDELP2, long double CABDEL, long double REFDEL, string REF, int &CK, int &ICK, string cop1, string cop2){

    int i,k,m, aux, place,aux3,varia;
    char  ABSTXT[80],  printar[30], ABSTXT2[80];
    string a;
    bool ok = true;
    long double m_ns = 299792458.0, aux2;
    aux2 = pow(10,-9);
    m_ns = m_ns * aux2;

    fstream LUOUT;
    LUOUT.open (FLOUT);
    if (LUOUT.good()){}
       else{
          cout << "WARNING:" << '\n';
          cout << "fail to open file unit " <<  FLOUT << '\n';
           };

    LUOUT << "CGGTTS GPS/GLONASS DATA FORMAT VERSION = 02" << endl;
    LUOUT << "REV DATE = " << REVDATE << '\n';
    LUOUT << "RCVR = ";
    LUOUT.width(30);
    memset(printar, '\0', RCVR.length());
    RCVR.copy(printar, RCVR.length() - 1);

    LUOUT << left << printar;
    LUOUT << "R2CGGTTS v" << SVERSION << '\n';
    if(ITESTC1 == 0){
       LUOUT << "CH = " << CH << " (GPS)" << '\n';
    };
    LUOUT << "IMS = " << RCVR << '\n';
    LUOUT << "LAB = " << LAB <<  '\n';
    if(ANTPOS[0]>0){
      LUOUT.setf(ios::fixed,ios::floatfield);
      LUOUT.precision(2);
      LUOUT << "X = +" <<  setprecision(2) <<  ANTPOS[0] << " m (GPS)" << '\n';
    }
    else{
        LUOUT << "X = " << /* setprecision(2) << */ ANTPOS[0] <<  " m (GPS)" << '\n';
      };
    if(ANTPOS[1]>0){
      LUOUT << "Y = +" << /* setprecision(2) << */ ANTPOS[1] << " m (GPS)" <<  '\n';
    }
    else{
        LUOUT << "Y = " << /* setprecision(2) << */ ANTPOS[1] <<  " m (GPS)" << '\n';
      };
    if(ANTPOS[2]>0){
      LUOUT << "Z = +" << /* setprecision(2) << */ ANTPOS[2] <<  " m (GPS)" << '\n';
    }
    else{
        LUOUT << "Z = " <</* setprecision(2) << */ ANTPOS[2] <<  " m (GPS)" << '\n';
      };
    LUOUT << "FRAME = ITRF, PZ-90->ITRF Dx = 0.0 m, Dy = 0.0 m, Dz = 0.0 m, ds = 0.0, Rx = 0.0, Ry = 0.0, Rz = 0.000000" << '\n';
    if(COMMENTS.length() < 60){
        COMMENTS.erase(remove(COMMENTS.begin(), COMMENTS.end(), '\r'), COMMENTS.end());
        COMMENTS.erase(remove(COMMENTS.begin(), COMMENTS.end(), '\n'), COMMENTS.end());
        LUOUT << "COMMENTS = " << COMMENTS + "          " + cop1 + "   " + cop2 <<'\n';
    }
    else{

        LUOUT << "COMMENTS = " << COMMENTS << '\n';
    }
    LUOUT << "INT DLY = ";
    LUOUT.setf(ios::fixed,ios::floatfield);
    LUOUT.precision(1);
    LUOUT.width(6);
    LUOUT << right << RECDELP1 / m_ns << " ns (GPS P1), ";
    LUOUT.precision(1);
    LUOUT.width(6);
    LUOUT << right << RECDELP2 / m_ns << " ns (GPS P2)" << '\n';
    LUOUT << "CAB DLY = ";
    LUOUT.precision(1);
    LUOUT.width(6);
    LUOUT << right << CABDEL / 10 << " ns (GPS)" << '\n';
    LUOUT << "REF DLY = ";
    LUOUT.precision(1);
    LUOUT.width(6);
    LUOUT << right << REFDEL / 10 << " ns" << '\n';
    LUOUT << "REF = ";
    LUOUT << left << REF <<'\n';

    LUOUT.seekg (0, ios::beg);
    ICK = 0;

    for(k=0; k<15; k++){
            a.clear();
            getline(LUOUT,a, '\n');
                aux3 = a.length();
                if(aux3>=50){
                    varia = 50;
                }
                else{
                    varia = aux3;
                };
                a.copy(ABSTXT2,varia);
            for(i=0;i<varia;i++){
                aux = ABSTXT2[i];
                if(aux>31){
                    ICK = ICK + aux;
                };
            }
        };


    ABSTXT[0] = 'C';
    ABSTXT[1] = 'K';
    ABSTXT[2] = 'S';
    ABSTXT[3] = 'U';
    ABSTXT[4] = 'M';
    ABSTXT[5] = ' ';
    ABSTXT[6] = '=';
    ABSTXT[7] = ' ';
    for(i = 7; i<50; i++){
        ABSTXT[i] = ' ';
    };
    for(i=1;i<7;i++){
        ICK = ICK + ABSTXT[i];
    };


    CK = (ICK % 256);

   LUOUT.seekg (0, ios::end);

    LUOUT <<ABSTXT[0]<<ABSTXT[1]<<ABSTXT[2]<<ABSTXT[3]<<ABSTXT[4]<<ABSTXT[5]<<ABSTXT[6]<<ABSTXT[7] << hex << CK << '\n';
    LUOUT << '\n';

    LUOUT << "PRN" << " " << "CL" << "  " << "MJD" << "  " << "STTIME" << " " << "TRKL" << " " << "ELV" << " " << "AZTH" << "   " << "REFSV" << "      ";

        LUOUT << "SRSV" << "     " << "REFSYS" << "    " << "SRSYS" << "  " << "DSG" << " " << "IOE" << " " << "MDTR" << " " << "SMDT" << " " << "MDIO" << " ";
        LUOUT << "SMDI MSIO SMSI ISG FR HC FRC CK PS1 PS2" << '\n';

        LUOUT << "             " << "hhmmss"<< "  "<<"s"<<"  "<< ".1dg "<< ".1dg"<< "    "<<".1ns"<<"     "<<".1ps/s"<<"     "<<".1ns"<<"    ";
        LUOUT<< ".1ps/s .1ns"<<"     "<< ".1ns.1ps/s.1ns.1ps/s.1ns.1ps/s.1ns"<<'\n';

        LUOUT.close();
};



void xyzlla (long double ANTPOS[], long double (&LLA)[3]){
    long double pi = 3.1415926535898, A = 6378137, DF = 0.00335281066, DX, DY, DZ, DA2, DB;
    long double DE, DE2, DL, DXY, DP, DN, DZP, DPH, DRES, DCP, DCL, DH;
    int L = 1;

    DX =  ANTPOS[0];
    DY =  ANTPOS[1];
    DZ =  ANTPOS[2];
    DA2 = A * A;
    DB = A * (1.0 - DF);
    DE = sqrt( (DA2 - (DB*DB)) / DA2);
    DE2 = DE * DE;
    DL = atan(DY/DX);

    if(DL < 0){
        goto seis;
    }
    else{
        goto tres;
    };
    tres:
    if(DX < 0){
        goto cinco;
    }
    else{
        goto oito;
    };
    cinco:
    DL = DL + pi;
    goto oito;
    seis:
    if(DX < 0){
        goto cinco;
    }
    else if(DX == 0){
        goto oito;
    }
    else{
        goto sete;
    };
    sete:
    DL = DL + (2* pi);
    oito:
    DXY = sqrt((DX * DX) + (DY * DY));
    DP = atan(DZ/DXY);
    L = 1;
    dez:
    DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
    if(L == 1){
        goto onze;
    };
    if(L == 2){
        goto vinte;
    };
    onze:
    DZP = DZ + DE2 * DN * sin(DP);
    DPH = atan(DZP / DXY);
    DRES = fabs(DP - DPH);
    if(DRES < 0.0001){
        goto quinze;
    };
    DP = DPH;
    goto dez;
    quinze:
    L = 2;
    DCP = cos(DPH);
    DCL = cos(DL);

    goto dez;
    vinte:
    DH = (DX / (DCP * DCL)) - DN;
    LLA[0] = DPH;
    LLA[1] = DL;
    LLA[2] = DH;

   return;
}



//Função igual â função acima, mas escrita com menos "goto"

/*
void xyzlla (long double ANTPOS[], long double (&LLA)[3]){
    //ofstream blabla2 ("arquivo_saida2.txt", ios::app);
    long double pi = 3.1415926535898, A = 6378137, DF = 0.00335281066, DX, DY, DZ, DA2, DB;
    long double DE, DE2, DL, DXY, DP, DN, DZP, DPH, DRES, DCP, DCL, DH;
    //const double pi = (atan(1))*4;
    DX =  ANTPOS[0];
    DY =  ANTPOS[1];
    DZ =  ANTPOS[2];
    DA2 = A * A;
    DB = A * (1.0 - DF);
    DE = sqrt( (DA2 - (DB*DB)) / DA2);
    DE2 = DE * DE;
    DL = atan(DY/DX);
    //blabla2<<setprecision(30)<<DZ<<endl;
//    cout.precision( numeric_limits<double>::digits10 + 1);
//    cout<<DL<<" DL"<<endl;

   if(DL < 0){
        if(DX < 0){
            DL = DL + pi;
            DXY = sqrt((DX * DX) + (DY * DY));
            DP = atan(DZ/DXY);
            //cout<<"1"<<endl;
            repete:
            DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
            DZP = DZ + DE2 * DN * sin(DP);
            DPH = atan(DZP / DXY);
            DRES = fabs(DP - DPH);
          abc:
            if(DRES < 0.0001){
                DCP = cosf(DPH);
                DCL = cosf(DL);
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DH = (DX / (DCP * DCL)) - DN;
                LLA[0] = DPH;
                LLA[1] = DL;
                LLA[2] = DH;
               // cout<<setprecision(30)<<((DCP * DCL))<<endl;
             return;
            }
            else{
                DP = DPH;
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DZP = DZ + DE2 * DN * sin(DP);
                DPH = atan(DZP / DXY);
                DRES = fabs(DP - DPH);
 //               cout<<"4"<<endl;
                goto abc;
            };

        }
        else if (DX == 0){
            DXY = sqrt((DX * DX) + (DY * DY));
            DP = atan(DZ/DXY);
            DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
            DZP = DZ + DE2 * DN * sin(DP);
            DPH = atan(DZP / DXY);
            DRES = fabs(DP - DPH);
   //         cout<<"5"<<endl;
            if(DRES < 0.0001){
                DCP = cos(DPH);
                DCL = cos(DL);
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DH = (DX / (DCP * DCL)) - DN;
                LLA[0] = DPH;
                LLA[1] = DL;
                LLA[2] = DH;
  //              cout<<"6"<<endl;
                return;
            }
            else{
                DP = DPH;
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DZP = DZ + DE2 * DN * sin(DP);
                DPH = atan(DZP / DXY);
                DRES = fabs(DP - DPH);
      //          cout<<"7"<<endl;
                goto abc;
            };
        }
        else{
            DL = DL + (2* pi);
            DXY = sqrt((DX * DX) + (DY * DY));
            DP = atan(DZ/DXY);
            DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
            DZP = DZ + DE2 * DN * sin(DP);
            DPH = atan(DZP / DXY);
            DRES = fabs(DP - DPH);
          if(DRES < 0.0001){
                DCP = cos(DPH);
                DCL = cos(DL);
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DH = (DX / (DCP * DCL)) - DN;
                LLA[0] = DPH;
                LLA[1] = DL;
                LLA[2] = DH;
//                cout<<"9"<<endl;
                return;
            }
            else{
                DP = DPH;
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DZP = DZ + DE2 * DN * sin(DP);
                DPH = atan(DZP / DXY);
                DRES = fabs(DP - DPH);
   //             cout<<"10"<<endl;
                goto abc;
            };
        };
    }
    else{
        if(DX < 0){
            DL = DL + pi;
            DXY = sqrt((DX * DX) + (DY * DY));
            DP = atan(DZ/DXY);
            DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
            DZP = DZ + DE2 * DN * sin(DP);
            DPH = atan(DZP / DXY);
            DRES = fabs(DP - DPH);
  //          cout<<"11"<<endl;
            if(DRES < 0.0001){
                DCP = cos(DPH);
                DCL = cos(DL);
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DH = (DX / (DCP * DCL)) - DN;
                LLA[0] = DPH;
                LLA[1] = DL;
                LLA[2] = DH;
   //             cout<<"12"<<endl;
                return;
            }
            else{
                DP = DPH;
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DZP = DZ + DE2 * DN * sin(DP);
                DPH = atan(DZP / DXY);
                DRES = fabs(DP - DPH);
  //              cout<<"13"<<endl;
                goto abc;
            };

        }
        else {
            DXY = sqrt((DX * DX) + (DY * DY));
            DP = atan(DZ/DXY);
            DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
            DZP = DZ + DE2 * DN * sin(DP);
            DPH = atan(DZP / DXY);
            DRES = fabs(DP - DPH);
   //         cout<<"14"<<endl;
            if(DRES < 0.0001){
                DCP = cos(DPH);
                DCL = cos(DL);
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DH = (DX / (DCP * DCL)) - DN;
                LLA[0] = DPH;
                LLA[1] = DL;
                LLA[2] = DH;
  //              cout<<"15"<<endl;
                return;
            }
            else{
                DP = DPH;
                DN = A / (sqrt(1.0 - ((DE * sin(DP)) * (DE * sin(DP)))));
                DZP = DZ + DE2 * DN * sin(DP);
                DPH = atan(DZP / DXY);
                DRES = fabs(DP - DPH);
   //             cout<<"16"<<endl;
                goto abc;
            };
        }
    };
};

*/

void azel(long double XS[], long double ANTPOS[], long double &AZ, long double &EL){
    long double pi=3.1415926535898, AX = 0.9966472;
    long double DX, DY, DZ, RANGE, CMRE, COSEL, LLA[3], CP, CL, SP, SL;

    DX = XS[0] - ANTPOS[0];
    DY = XS[1] - ANTPOS[1];
    DZ = XS[2] - ANTPOS[2];

    RANGE = sqrt((DX * DX) + (DY * DY) + (DZ * DZ));
    CMRE =sqrt((ANTPOS[0] * ANTPOS[0]) + (ANTPOS[1] * ANTPOS[1]) + (ANTPOS[2] * ANTPOS[2]));
    COSEL = ((DX * ANTPOS[0]) + (DY * ANTPOS[1]) + (DZ * ANTPOS[2])) / (CMRE * RANGE);

    if(COSEL < 0){
        EL = 0;
    }
    else{
        EL = (pi / 2.0) - acos(COSEL);
    };

    xyzlla(ANTPOS, LLA);

    CP = cos(LLA[0]);
    CL = cos(LLA[1]);
    SP = sin(LLA[0]);
    SL = sin(LLA[1]);
    AZ = atan2(((-DX * SL) + (DY * CL)),((-DX * SP * CL)-(DY * SP * SL) + (DZ * CP * AX)));
    if(AZ < 0){
         AZ = AZ + (2 * pi);
    };

    return;

};





void satpos(long double BRORB[3000][7][4], long double TTR, long double &TREL, long double (&X)[3],  bool stat, int jkl ,int SATNUM, char CODE){

    long double pi = 3.1415926535898, mu = 3.986005, wedot = 7.2921151467, f = -4.442807633;
    long double aux, auxil, T, N0, F, WC, XDASH, YDASH, I, R, U, DI, DR, DU, PHI, NU;
    long double M0, DN, W0, I0, W, WDOT, IDOT, CIC, M, N, E, EOLD, SNU, CNU;
    long double CRS, CUC, EC, CUS, A, TOE, CIS, CRC;
    long double RX[3][3], RZ[3][3], MAT_RES[3][3], XG[3];
    int IT,llll, oooo, mmmm;


    aux = pow( 10, 14);
    mu = mu * aux;
    aux = pow( 10, -5);
    wedot = wedot * aux;
    aux = pow( 10, -10);
    f = f * aux;


    M0   = BRORB[jkl][0][3];
    DN   = BRORB[jkl][0][2];
    W0   = BRORB[jkl][2][2];
    I0   = BRORB[jkl][3][0];
    W    = BRORB[jkl][3][2];
    WDOT = BRORB[jkl][3][3];
    IDOT = BRORB[jkl][4][0];


    CRS = BRORB[jkl][0][1];
    CUC = BRORB[jkl][1][0];
    EC  = BRORB[jkl][1][1];
    CUS = BRORB[jkl][1][2];
    A   = (BRORB[jkl][1][3] * BRORB[jkl][1][3]);
    TOE = BRORB[jkl][2][0];
    CIC = BRORB[jkl][2][1];
    CIS = BRORB[jkl][2][3];
    CRC = BRORB[jkl][3][1];


   stat = true;

   IT = 0;

   T = TTR - TOE;

   if(T > 302400){
       T = T -  604800;
   };
   if(T < (-302400)){
       T = T + 604800;
   };

   aux = mu/(A*A*A);
   N0 = sqrt(aux);
   N = N0 + DN;
   M = M0 + (N * T);
   E = M;


   lll:
   IT++;


   EOLD = E;
   E = M + EC * sin(E);
   aux = pow(10, -8);
   auxil = fabs(E - EOLD);

   if((IT == 10) || (auxil <= aux)){
        goto op;
   };
   goto lll;
   op:
   if(IT == 10){
        stat = false;
        cout << "no convergence for E" << '\n';
        return;
   };

    SNU = sqrt(1.0 - (EC*EC)) * (sin(E) / (1.0 - (EC * cos(E))));
    CNU = (cos(E) - EC) / (1.0 - (EC*cos(E)));
    NU = atan2(SNU, CNU);


    PHI =  NU + W;

    DU = (CUC * cos(2.0 * PHI)) + (CUS * sin(2.0 * PHI));
    DR = (CRC * cos(2.0 * PHI)) + (CRS * sin(2.0 * PHI));
    DI = (CIC * cos(2.0 * PHI)) + (CIS * sin(2.0 * PHI));

    U = PHI + DU;
    R = A * (1.0 -  EC*cos(E))+ DR;
    I = I0 + (IDOT * T) + DI;

    XDASH = R * cos(U);
    YDASH = R * sin(U);

    if((CODE == 'C')&&(( SATNUM > 210) && ( SATNUM <= 214))&&( SATNUM > 200)&&( SATNUM < 300)){
        WC = W0 + ((WDOT - wedot) * T) - (wedot * TOE);
    }
    else if ((CODE == 'C')&&( SATNUM > 200)&&( SATNUM < 300)){
        WC = W0 + ((WDOT) * T) - (wedot * TOE);
    }
    else{
        WC = W0 + ((WDOT - wedot) * T) - (wedot * TOE);
    }

    X[0] = (XDASH * cos(WC)) - (YDASH * cos(I) * sin(WC));
    X[1] = (XDASH * sin(WC)) + (YDASH * cos(I) * cos(WC));
    X[2] = YDASH * sin(I);


    if((CODE == 'C') && ((SATNUM >200) && ((SATNUM<=210)||(SATNUM>214))) &&(SATNUM<300)){


        XG[0] = (X[0]);
        XG[1] = X[1];
        XG[2] = X[2];

        RZ[0][0] = 1;
        RZ[0][1] = 0;
        RZ[0][2] = 0;
        RZ[1][0] = 0;
        RZ[1][1] = cos(-5);
        RZ[1][2] = sin(-5);
        RZ[2][0] = 0;
        RZ[2][1] = -(sin(-5));
        RZ[2][2] = cos(-5);

        RX[0][0] = cos(WDOT * T);
        RX[0][1] = sin(WDOT * T);
        RX[0][2] = 0;
        RX[1][0] = -(sin((WDOT * T)));
        RX[1][1] = cos(WDOT * T);
        RX[1][2] = 0;
        RX[2][0] = 0;
        RX[2][1] = 0;
        RX[2][2] = 1;

         for(mmmm= 0; mmmm < 3; mmmm++){
            for(llll = 0; llll < 3;llll++){

                MAT_RES[mmmm][llll] = 0;
                for(oooo = 0; oooo < 3; oooo++){
                    MAT_RES[mmmm][llll] = MAT_RES[mmmm][llll] + (RZ[mmmm][oooo] * RX[oooo][llll]);
                }
            }
         }

         for(mmmm= 0; mmmm < 3; mmmm++){
              X[mmmm] = 0;
            for(llll = 0; llll < 3; llll ++){
                X[mmmm] = ((MAT_RES[mmmm][llll]) * (XG[llll]) )+ X[mmmm];
            }
         }
    }


    TREL = f * EC *BRORB[jkl][1][3] * sin(E);


};



void fitlin (long double TIME[], long double VECT[], long double &VALMIL, long double &DERIV, long double &STDV,int LEAPSEC, float MINUTOS_VETOR){
    long double  S, SX, SXY, SXX, SY, CAL, TMIL, T1, ORD, ABSCISSE, DELTA, A, B, ESTIM;
    int j;


    STDV = 0;
    S = 0;
    SX = 0;
    SXY = 0;
    SXX = 0;
    SY = 0;
    CAL = 1000.0;
    //0.00451388888888888888888888888888888927
    TMIL = TIME[0] + (((MINUTOS_VETOR/2.0)/24.0)/60.0) - (0.000347222222222222222222222222222222234) + double(LEAPSEC) / 86400.0;
    T1 = TIME[0];

    for(j = 0; j < (MINUTOS_VETOR * 2); j++){
        ABSCISSE = TIME[j] - T1;

        if(VECT[j] != 0){
            ORD = (VECT[j] - VECT[0]) / CAL;
            S = S + 1.0;
            SX = SX + ABSCISSE;
            SXX = SXX + (ABSCISSE * ABSCISSE);
            SY = SY + ORD;
            SXY = SXY + (ORD * ABSCISSE);

        };
    };

    if(S > 1){
        DELTA = (SXX * S) - (SX * SX);
        A = ((SY * SXX) - (SX * SXY)) / DELTA;
        B = ((S* SXY) - (SX * SY)) / DELTA;


    }
    else{
        A = 98;
        B = 98;
    };

    if( A != 98){
        VALMIL = ((A + B*(TMIL - T1)) * CAL) + VECT[0];
        DERIV = B * CAL;
        STDV = 0;

        for(j = 0; j < (MINUTOS_VETOR * 2); j++){
            ESTIM = ((A + B * (TIME[j] - T1)) * CAL) + VECT[0];
            STDV = STDV + ((VECT[j] - ESTIM) * (VECT[j] - ESTIM));
        };
        STDV = sqrt(STDV / (S - 2));
    }
    else{
        VALMIL = 9999999;
        DERIV = 9999999;
        STDV = 9999999;
    };

};


int main(){


int ICOUNT = 0, FR = 0, HC = 0,kkk=0, ii1 = 0;

long double MJD = -1, SVERSION = 5.1;

const char FPARA[] = "paramCGGTTS.dat"; //		parameter file
const char FINP[] = "inputFile.dat";
 char FNAV[20]= "rinex_nav";  //     navigation file
 char FNAVP[20]= "rinex_nav_p";//     navigation file next day
 char FOBS[20]= "rinex_obs";//     observation file
 char FOBSP[20] = "rinex_obs_p";//     observation file next day
 char FLOUT[20]= "cggtts.gps";//     GPS output file
 char FLLOG[20]= "cggtts.log";//     Log output file

 char FLMOUT[20]="CGGTTS.mix" , GNAVP[512],GNAV[512],FLGOUT[512] ;
 char FRC[] = "L3P";
string PARAM="", teste;



//open parameter file must exist
ifstream LUPARA;
LUPARA.open (FPARA);
if (LUPARA.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FPARA << '\n';
       };


// open input file if it exists
ifstream LUINP;
LUINP.open (FINP);
if (LUINP.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FINP << '\n';
       };

while(getline(LUINP, PARAM, '\n')){
 if(PARAM.compare(0,20,"FILE_RINEX_NAV_GLO_P")==0){
    LUINP >> GNAVP;
    ICOUNT++;
                                                  }
 else if(PARAM.compare(0,18, "FILE_RINEX_NAV_GLO")==0){
    LUINP >> GNAV;
 }

 else if(PARAM.compare(0,16,"FILE_RINEX_NAV_P")==0){
    getline(LUINP, teste, '\n');
    memset(FNAVP, '\0', teste.length());
    teste.copy(FNAVP, teste.length());
    teste.clear();
    ICOUNT++;
                                               }
 else if(PARAM.compare(0,14, "FILE_RINEX_NAV")==0){
     getline(LUINP, teste, '\n');
     memset(FNAV, '\0', teste.length());
     teste.copy(FNAV, teste.length());
     teste.clear();
     ICOUNT++;
 }
 else if(PARAM.compare(0,16,"FILE_RINEX_OBS_P")==0){
    getline(LUINP, teste, '\n');
    memset(FOBSP, '\0', teste.length());
    teste.copy(FOBSP, teste.length());
    teste.clear();
    ICOUNT++;
 }
 else if(PARAM.compare(0,14,"FILE_RINEX_OBS")==0){
     getline(LUINP, teste, '\n');
   memset(FOBS, '\0', teste.length());
   teste.copy(FOBS, teste.length());
   teste.clear();
    ICOUNT++;
 }
 else if(PARAM.compare(0,17,"FILE_CGGTTS_GLO")==0){
      getline(LUINP, teste, '\n');
    memset(FLGOUT, '\0', teste.length());
    teste.copy(FLGOUT, teste.length());
    teste.clear();
 }
 else if (PARAM.compare(0,15,"FILE_CGGTTS_LOG")==0){
     getline(LUINP, teste, '\n');
   memset(FLLOG, '\0', teste.length());
   teste.copy(FLLOG, teste.length());
   teste.clear();
 }
 else if(PARAM.compare(0,15,"FILE_CGGTTS_OUT")==0){
    getline(LUINP, teste, '\n');
    memset(FLOUT, '\0', teste.length());
    teste.copy(FLOUT, teste.length());
    teste.clear();
 }
 else if(PARAM.compare(0,15,"FILE_CGGTTS_MIX")==0){
     getline(LUINP, teste, '\n');
     memset(FLMOUT, '\0', teste.length());
     teste.copy(FLMOUT, teste.length());
     teste.clear();

 }
 else if(PARAM.compare(0,19,"MODIFIED_JULIAN_DAY")==0){
    LUINP >> MJD;
    ICOUNT++;
                                                  };
                                         };

ifstream LULOG;
LULOG.open (FLLOG);
if (LULOG.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FLLOG << '\n';
       };

//Pulei a parte do programa que escreve a versão do programa (tanto na tela quanto no file log (na real, pulei tudo que escreve no file log))

long double WE = 7.292115;       //frequencia angular da rotacao da terra
long double auxiliar;
long double VITLUM=299792458.0;  // velocidade da luz
long double NS, DeltaN,NSLOG, pi=3.1415926535898;
//float VITLUM=299792458.0;

auxiliar = pow(10,-5);
WE = WE * auxiliar;     //apenas para colocar na notação certa

//tropo parameters

NS = 324.8;
auxiliar = 0.005577 * NS;
auxiliar = pow(2.71828182845904523,auxiliar);
DeltaN = (-7.32*auxiliar);
NSLOG = log((NS+DeltaN)/105);

string REVDATE, LAB, COMMENTS,FRAME,REF;
int CH=0,LEAPSEC=0;
long double ANTPOS[3], RECDELP1 = 0.0, RECDELP2 = 0, RECDELP1GLO = 0,RECDELP2GLO = 0, CABDEL = 0,REFDEL = 0;
string RCVR;
char CODE;

while(getline(LUPARA, PARAM, '\n')){
 if(PARAM.compare(0,8,"REV DATE")==0){
    LUPARA >> REVDATE;
 }
 else if(PARAM.compare(0,4,"RCVR")==0){

    getline(LUPARA, RCVR, '\n');
                                   }
 else if(PARAM.compare(0,2,"CH")==0){
    LUPARA >> CH;
                              }
 else if(PARAM.compare(0,8,"LAB NAME")==0){
    LUPARA >> LAB;
                                    }
 else if(PARAM.compare(0,12,"X COORDINATE")==0){
    LUPARA >> ANTPOS[0];
                                             }
 else if(PARAM.compare(0,12,"Y COORDINATE")==0){
    LUPARA >> ANTPOS[1];
                         }
 else if(PARAM.compare(0,12,"Z COORDINATE")==0){
    LUPARA >> ANTPOS[2];
                                             }
 else if(PARAM.compare(0,8,"COMMENTS")==0){
    getline(LUPARA, COMMENTS, '\n');
                                        }
 else if(PARAM.compare(0,5,"FRAME")==0){
    LUPARA >> FRAME;
                     }
 else if(PARAM.compare(0,3,"REF")==0){
    LUPARA >> REF;
                                   }
 else if(PARAM.compare(0,26,"INT DELAY P1 XR+XS (in ns)")==0){
    LUPARA >> RECDELP1;
                                                      }
 else if(PARAM.compare(0,24,"INT DELAY P1 GLO (in ns)")==0){
    LUPARA >> RECDELP1GLO;
                                 }
 else if(PARAM.compare(0,26,"INT DELAY C1 XR+XS (in ns)")==0){
    LUPARA >> RECDELP1;
                               }
 else if(PARAM.compare(0,26,"INT DELAY P2 XR+XS (in ns)")==0){
    LUPARA >> RECDELP2;
                                   }
 else if(PARAM.compare(0,24,"INT DELAY P2 GLO (in ns)")==0){
    LUPARA >> RECDELP2GLO;
                               }
 else if(PARAM.compare(0,21,"ANT CAB DELAY (in ns)")==0){
    LUPARA >> CABDEL;
                              }
 else if(PARAM.compare(0,11,"LEAP SECOND")==0){
    LUPARA >> LEAPSEC;
                        }
 else if(PARAM.compare(0,29,"CLOCK CAB DELAY XP+XO (in ns)")==0){
    LUPARA >> REFDEL;
                        };

                                               };

ofstream blabla;
blabla.open(FLOUT);
if (blabla.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FLOUT << '\n';
       };

fstream LUOUT;
LUOUT.open (FLOUT);
if (LUOUT.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FLOUT << '\n';
       };

LUOUT.close();

ifstream LUMOUT;
LUMOUT.open (FLMOUT);
if (LUMOUT.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FLMOUT << '\n';
       };

long double COEF1 = 0, COEF2 = 0, ILEAPS=0, MJDDEP = 50722.0;
int NSCH = 89, ITS, MI,HE,  /*SCHH[100],SCHMIN[100], */i , Tempo_obser_calculo = -1, Tempo_obser = -1;



cout << "Enter with  the time (in seconds) used between MJDs."<<endl;
cin >> Tempo_obser;
//Tempo_obser = 16;
Tempo_obser_calculo = (86400/(Tempo_obser*60));


cout << "Enter with the constellation code: G, R, E, C" << endl;
cin >> CODE;
//CODE = 'E';
CODE = toupper(CODE);


float MINUTOS_VETOR = 0;
cout << "Enter with  the number of observations used to construct the vectors. "<<endl;
cin >> MINUTOS_VETOR;
//MINUTOS_VETOR = 13.00;

string cop1 = "C5Q", cop2="C7Q" , coc1 = "C1t", coc2="C2t";




vector<int> SCHH;
vector<int> SCHMIN;
vector<long double>SCHTIME;

SCHH.resize(Tempo_obser_calculo + 1);
SCHMIN.resize(Tempo_obser_calculo + 1);
SCHTIME.resize(Tempo_obser_calculo + 1);

//Delay da Antena
RECDELP1 = RECDELP1*VITLUM*pow(10,-9);
RECDELP2 = RECDELP2*VITLUM*pow(10,-9);
RECDELP1GLO = RECDELP1GLO*VITLUM*pow(10,-9);
RECDELP2GLO = RECDELP2GLO*VITLUM*pow(10,-9);
COEF1 = 1.0/(1.0-(49.0/81.0));
COEF2 = 1.0/((81.0/49.0)-1.0);


//Delay do cabo
CABDEL=CABDEL*10;
REFDEL=REFDEL*10;  //Delay do receptor

ILEAPS = LEAPSEC/30;
ILEAPS = (ILEAPS+1)*30;


if(MJD==(-1)){
 cout << "Enter MJD ";
 cin >> MJD;
 cout << '\n';
          };


for(i=0;i<Tempo_obser_calculo-1; i++){
    ITS = 2 + Tempo_obser*(i);
    MI = ITS%60;
    HE=ITS/60;

    decal(MJD,MJDDEP,HE,MI,SCHH,SCHMIN,i);

    SCHTIME[i] = ( MJD + (SCHH[i]/24.0) + (SCHMIN[i]/(24*60.0)) + (ILEAPS/86400.0) );

};


classement (SCHH,SCHMIN,SCHTIME,Tempo_obser_calculo-1);

NSCH = Tempo_obser_calculo-1;

if(SCHMIN[Tempo_obser_calculo-2] < (60-Tempo_obser)){
    NSCH = Tempo_obser_calculo;
    SCHH[Tempo_obser_calculo-1] = SCHH[Tempo_obser_calculo-2];
    SCHMIN[Tempo_obser_calculo-1] = SCHMIN[Tempo_obser_calculo-2] + Tempo_obser;
    SCHTIME[Tempo_obser_calculo-1] = SCHTIME[Tempo_obser_calculo-2] + (float(Tempo_obser)/1440.0);
};


ifstream LUFOBS;

LUFOBS.open (FOBS);
if (LUFOBS.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FOBS << '\n';
       };


//abre o arquivo de observacao do dia seguinte
ifstream LUFOBP;
LUFOBP.open (FOBSP);
if (LUFOBP.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FOBSP << '\n';
       };


//abre arquivo de navegacao
ifstream LUNAV;
LUNAV.open (FNAV);
if (LUNAV.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FNAV << '\n';
       };


//abre arquivo de navegacao do proximo dia
ifstream LUNAV2P;
LUNAV2P.open (FNAVP);
if (LUNAV2P.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FNAVP << '\n';
       };


//***********************************************reading of GPS ephemerides***************************************************


long double DATENAV[3000], SVCLK[3000][3], BRORB[3000][7][4], SECN, TIMENAV,t1;
int PPRN[3000], k,l,jkl,j,jj;
int PRN,YN,MN,DN,HN,MINN,ak;
string COMMENT="", strin2, ajuda2;

for(jkl=0; jkl<3000; jkl++){
    DATENAV[jkl] = 0.0;
    PPRN[jkl] = 0;
    for(k=0; k<3; k++){
        SVCLK[jkl][k] = 0.0;
    };
    for(k=0; k<7; k++){
        for(l=0; l<4; l++){
            BRORB[jkl][k][l] = 0.0;
        };
    };
};


if(LUNAV.good()){
    while(getline(LUNAV, COMMENT, '\n')){
        if(COMMENT.compare(0,73,"                                                            END OF HEADER")==0){
            break;
        };
    };

    i=0;
    strin2.clear();
    while((i<=1499)&&(getline(LUNAV,strin2,'\n'))){
        if(LUNAV.eof()){
            break;
        }
        t1 = count(strin2.begin(), strin2.end(), 'D');
        for(ii1=0;ii1<t1;ii1++){
           strin2.replace(strin2.find("D"), 1, "E");
        }

        ajuda2 = strin2.substr (1,2);
        istringstream (ajuda2) >> PRN;

         ajuda2 = strin2.substr (0,1);


        if((CODE=='G')&&(strin2.compare(0,1,"G")==0)){

            ajuda2 = strin2.substr (4,4);
            istringstream (ajuda2) >> YN;

            ajuda2 = strin2.substr (8,3);
            istringstream (ajuda2) >> MN;

            ajuda2 = strin2.substr (11,3);
            istringstream (ajuda2) >> DN;

            ajuda2 = strin2.substr (14,3);
            istringstream (ajuda2) >> HN;

            ajuda2 = strin2.substr (17,3);
            istringstream (ajuda2) >> MINN;

            ajuda2 = strin2.substr  (21,2);
            istringstream (ajuda2) >> SECN;

             ajuda2 = strin2.substr (23,19);
             istringstream (ajuda2) >> SVCLK[i][0];

            ajuda2 = strin2.substr (42,19);
            istringstream (ajuda2) >> SVCLK[i][1];

            ajuda2 = strin2.substr (61,19);
            istringstream (ajuda2) >> SVCLK[i][2];

            datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
            DATENAV[i] = TIMENAV;

            PPRN[i] = PRN;

            for(j=0; j<7; j++){
                strin2.clear();
                getline(LUNAV,strin2,'\n');

                t1 = count(strin2.begin(), strin2.end(), 'D');

                if(j < 6){
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }
                    for(jj = 0; jj < 4; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                            istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                     };

                }
                else{
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }

                    for(jj = 0; jj < 2; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                             istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                    };

                };
            };

            i++;

        }
        else if((CODE == 'C')&&(strin2.compare(0,1,"C")==0)){

            ajuda2 = strin2.substr (4,4);
            istringstream (ajuda2) >> YN;

            ajuda2 = strin2.substr (8,3);
            istringstream (ajuda2) >> MN;

            ajuda2 = strin2.substr (11,3);
            istringstream (ajuda2) >> DN;

            ajuda2 = strin2.substr (14,3);
            istringstream (ajuda2) >> HN;

            ajuda2 = strin2.substr (17,3);
            istringstream (ajuda2) >> MINN;

            ajuda2 = strin2.substr  (21,2);
            istringstream (ajuda2) >> SECN;

            ajuda2 = strin2.substr (23,19);
            istringstream (ajuda2) >> SVCLK[i][0];

            ajuda2 = strin2.substr (42,19);
            istringstream (ajuda2) >> SVCLK[i][1];

            ajuda2 = strin2.substr (61,19);
            istringstream (ajuda2) >> SVCLK[i][2];

            datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
            DATENAV[i] = TIMENAV;

            PPRN[i] = PRN;

            for(j=0; j<7; j++){
                strin2.clear();
                getline(LUNAV,strin2,'\n');

                t1 = count(strin2.begin(), strin2.end(), 'D');

                if(j < 6){
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }
                    for(jj = 0; jj < 4; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                            istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                    };

                }
                else{
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }

                    for(jj = 0; jj < 2; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                           istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                    };

                };

            };

            i++;

        }
        else if((CODE=='E')&&(strin2.compare(0,1,"E")==0)){

            ajuda2 = strin2.substr (4,4);
            istringstream (ajuda2) >> YN;

            ajuda2 = strin2.substr (8,3);
            istringstream (ajuda2) >> MN;

            ajuda2 = strin2.substr (11,3);
            istringstream (ajuda2) >> DN;

            ajuda2 = strin2.substr (14,3);
            istringstream (ajuda2) >> HN;

            ajuda2 = strin2.substr (17,3);
            istringstream (ajuda2) >> MINN;

            ajuda2 = strin2.substr  (21,2);
            istringstream (ajuda2) >> SECN;

             ajuda2 = strin2.substr (23,19);
             istringstream (ajuda2) >> SVCLK[i][0];

            ajuda2 = strin2.substr (42,19);
            istringstream (ajuda2) >> SVCLK[i][1];

            ajuda2 = strin2.substr (61,19);
            istringstream (ajuda2) >> SVCLK[i][2];

            datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
            DATENAV[i] = TIMENAV;

            PPRN[i] = PRN;



            for(j=0; j<7; j++){
                strin2.clear();
                getline(LUNAV,strin2,'\n');

                t1 = count(strin2.begin(), strin2.end(), 'D');

                if(j < 6){
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }
                    for(jj = 0; jj < 4; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                            istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                     };

                }
                else{
                    for(ii1=0;ii1<t1;ii1++){
                        strin2.replace(strin2.find("D"), 1, "E");
                    }

                    for(jj = 0; jj < 2; jj++){
                        BRORB[i][j][jj] = 0;
                        ajuda2 = strin2.substr (4+(jj*19),19);
                        if(ajuda2 != "                   "){
                             istringstream (ajuda2) >> BRORB[i][j][jj];
                        };
                    };

                };
            };

            i++;

        }
        else if(strin2.compare(0,1,"J")==0){
            for(ak=0;ak<7;ak++){
                getline(LUNAV, ajuda2, '\n');
            };
        }
        else if((strin2.compare(0,1,"R")==0)||(strin2.compare(0,1,"S")==0)){
            for(ak=0;ak<3;ak++){
                getline(LUNAV, ajuda2, '\n');
            };
        };
    ajuda2.clear();
    };
};

if(LUNAV2P.good()){
    while(getline(LUNAV2P, COMMENT, '\n')){
        i++;
        if(COMMENT.compare(0,73,"                                                            END OF HEADER")==0){
           break;
        };
    };
};

strin2.clear();
while((i<=2999)&&(getline(LUNAV2P,strin2,'\n'))){
    if(LUNAV2P.eof()){
        break;
    }

    t1 = count(strin2.begin(), strin2.end(), 'D');
    for(ii1=0;ii1<t1;ii1++){
        strin2.replace(strin2.find("D"), 1, "E");
    }

    ajuda2 = strin2.substr (1,2);
    istringstream (ajuda2) >> PRN;

    if((CODE=='G')&&(strin2.compare(0,1,"G")==0)){
        ajuda2 = strin2.substr (4,4);
        istringstream (ajuda2) >> YN;

        ajuda2 = strin2.substr (8,3);
        istringstream (ajuda2) >> MN;

        ajuda2 = strin2.substr (11,3);
        istringstream (ajuda2) >> DN;

        ajuda2 = strin2.substr (14,3);
        istringstream (ajuda2) >> HN;

        ajuda2 = strin2.substr (17,3);
        istringstream (ajuda2) >> MINN;

        ajuda2 = strin2.substr  (21,2);
        istringstream (ajuda2) >> SECN;

        ajuda2 = strin2.substr (23,19);
        istringstream (ajuda2) >> SVCLK[i][0];

        ajuda2 = strin2.substr (42,19);
        istringstream (ajuda2) >> SVCLK[i][1];

        ajuda2 = strin2.substr (61,19);
        istringstream (ajuda2) >> SVCLK[i][2];

        datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
        DATENAV[i] = TIMENAV;

        PPRN[i] = PRN;

        for(j=0; j<7; j++){
            strin2.clear();
            getline(LUNAV2P,strin2,'\n');

            t1 = count(strin2.begin(), strin2.end(), 'D');

            if(j < 6){
                for(ii1=0;ii1<t1;ii1++){
                    strin2.replace(strin2.find("D"), 1, "E");
                }
                for(jj = 0; jj < 4; jj++){
                    BRORB[i][j][jj] = 0;
                    ajuda2 = strin2.substr (4+(jj*19),19);
                    if(ajuda2 != "                   "){
                        istringstream (ajuda2) >> BRORB[i][j][jj];
                    };
                };
            }
            else{
                for(ii1=0;ii1<t1;ii1++){
                    strin2.replace(strin2.find("D"), 1, "E");
                }

                for(jj = 0; jj < 2; jj++){
                    BRORB[i][j][jj] = 0;
                    ajuda2 = strin2.substr (4+(jj*19),19);
                    if(ajuda2 != "                   "){
                        istringstream (ajuda2) >> BRORB[i][j][jj];
                    };
                };

            };
        };
        i++;
    }
    else if((CODE == 'C')&&(strin2.compare(0,1,"C")==0)){
        ajuda2 = strin2.substr (4,4);
        istringstream (ajuda2) >> YN;

        ajuda2 = strin2.substr (8,3);
        istringstream (ajuda2) >> MN;

        ajuda2 = strin2.substr (11,3);
        istringstream (ajuda2) >> DN;

        ajuda2 = strin2.substr (14,3);
        istringstream (ajuda2) >> HN;

        ajuda2 = strin2.substr (17,3);
        istringstream (ajuda2) >> MINN;

        ajuda2 = strin2.substr  (21,2);
        istringstream (ajuda2) >> SECN;

        ajuda2 = strin2.substr (23,19);
        istringstream (ajuda2) >> SVCLK[i][0];

        ajuda2 = strin2.substr (42,19);
        istringstream (ajuda2) >> SVCLK[i][1];

        ajuda2 = strin2.substr (61,19);
        istringstream (ajuda2) >> SVCLK[i][2];


        datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
        DATENAV[i] = TIMENAV;

        PPRN[i] = PRN;

        for(j=0; j<7; j++){
          strin2.clear();
          getline(LUNAV2P,strin2,'\n');

          t1 = count(strin2.begin(), strin2.end(), 'D');

          if(j < 6){
            for(ii1=0;ii1<t1;ii1++){
                strin2.replace(strin2.find("D"), 1, "E");
            }
            for(jj = 0; jj < 4; jj++){
                BRORB[i][j][jj] = 0;
                ajuda2 = strin2.substr (4+(jj*19),19);
                if(ajuda2 != "                   "){
                    istringstream (ajuda2) >> BRORB[i][j][jj];
                };
            };
          }
          else{
            for(ii1=0;ii1<t1;ii1++){
                strin2.replace(strin2.find("D"), 1, "E");
             }

             for(jj = 0; jj < 2; jj++){
                BRORB[i][j][jj] = 0;
                ajuda2 = strin2.substr (4+(jj*19),19);
                if(ajuda2 != "                   "){
                    istringstream (ajuda2) >> BRORB[i][j][jj];
                };
             };
            };
        };

     i++;
    }
    else if((CODE=='E')&&(strin2.compare(0,1,"E")==0)){
        ajuda2 = strin2.substr (4,4);
        istringstream (ajuda2) >> YN;

        ajuda2 = strin2.substr (8,3);
        istringstream (ajuda2) >> MN;

        ajuda2 = strin2.substr (11,3);
        istringstream (ajuda2) >> DN;

        ajuda2 = strin2.substr (14,3);
        istringstream (ajuda2) >> HN;

        ajuda2 = strin2.substr (17,3);
        istringstream (ajuda2) >> MINN;

        ajuda2 = strin2.substr  (21,2);
        istringstream (ajuda2) >> SECN;

        ajuda2 = strin2.substr (23,19);
        istringstream (ajuda2) >> SVCLK[i][0];

        ajuda2 = strin2.substr (42,19);
        istringstream (ajuda2) >> SVCLK[i][1];

        ajuda2 = strin2.substr (61,19);
        istringstream (ajuda2) >> SVCLK[i][2];

        datemjd(YN,MN,DN,HN,MINN,SECN,TIMENAV,1,CODE);
        DATENAV[i] = TIMENAV;

        PPRN[i] = PRN;

        for(j=0; j<7; j++){
            strin2.clear();
            getline(LUNAV2P,strin2,'\n');

            t1 = count(strin2.begin(), strin2.end(), 'D');

            if(j < 6){
                for(ii1=0;ii1<t1;ii1++){
                    strin2.replace(strin2.find("D"), 1, "E");
                }
                for(jj = 0; jj < 4; jj++){
                    BRORB[i][j][jj] = 0;
                    ajuda2 = strin2.substr (4+(jj*19),19);
                    if(ajuda2 != "                   "){
                        istringstream (ajuda2) >> BRORB[i][j][jj];
                    };
                };
            }
            else{
                for(ii1=0;ii1<t1;ii1++){
                    strin2.replace(strin2.find("D"), 1, "E");
                }

                for(jj = 0; jj < 2; jj++){
                    BRORB[i][j][jj] = 0;
                    ajuda2 = strin2.substr (4+(jj*19),19);
                    if(ajuda2 != "                   "){
                        istringstream (ajuda2) >> BRORB[i][j][jj];
                    };
                };

            };
        };
        i++;
    }
    else if ((strin2.compare(0,1,"J")==0)){
        for(ak=0;ak<7;ak++){
            getline(LUNAV2P, ajuda2, '\n');
        };
    }
    else if((strin2.compare(0,1,"R")==0)||(strin2.compare(0,1,"S")==0)){
        for(ak=0;ak<3;ak++){
            getline(LUNAV2P, ajuda2, '\n');
        };
    };
    ajuda2.clear();

};



//**********************************Leitura dos arquivos de observacao*****************************

int ITESTC1 =0, COLP1A =-1, COLP2A =-1, COLCA =-1, COLC2A =-1, COLC2B =-1, COLP1B =-1, COLP2B =-1, COLCAB =-1, A;
int NBOBSA = 0, NBOBSB = 0;

long double IFORMA, lixo2, IFORMB, LAST, DS, BIAS[40], BIAS2[40] ;


long double FREQ1_FINAL=0,FREQ2_FINAL=0;
string lixo, NAME, op;

char FTYPE, TSCALE[4], OBS[3][18],hhhhh[3];

bool OFSET=9;


if(LUFOBS.good()){
    LUFOBS >> IFORMA >> lixo >> lixo >> FTYPE;
    lixo2 = modf(IFORMA,&IFORMA);

    LUFOBS.clear();
    LUFOBS.seekg (0, ios::beg);



    while(getline(LUFOBS, PARAM,'\n')){

        op = PARAM.substr (61,77);
        if(PARAM.compare(60,19,"RCV CLOCK OFFS APPL")==0){


               lixo = PARAM.substr (6,1);
               istringstream (lixo) >> A;


               if(A==1){
                   OFSET = true;
               }

        }

        else if(PARAM.compare(60,13,"TIME OF FIRST")==0){
            lixo = PARAM.substr (48,3);
            lixo.copy(TSCALE,3);  //A priori sempre GPS


        }
        else if(PARAM.compare(60,19,"SYS / # / OBS TYPES")==0){

            string Code1, Code2, constellationID;
            lixo = PARAM.substr (0,1);

            constellationID = lixo;

             if((constellationID.compare(0,1,&CODE)==0))
           {
             lixo = PARAM.substr (4,2);
             istringstream (lixo) >> NBOBSA;


              if(NBOBSA<13){
                for(i=0;i<NBOBSA;i++){
                    lixo = PARAM.substr (7 +(4 * i),4);
                    lixo.copy(hhhhh,4);
                    OBS[0][i] = hhhhh[0];
                    OBS[1][i] = hhhhh[1];
                    OBS[2][i] = hhhhh[2];

                }

              }

              if(NBOBSA>13){
                for(i=0;i<13;i++){
                    lixo = PARAM.substr (7 +(4 * i),4);
                    lixo.copy(hhhhh,3);
                    OBS[0][i] = hhhhh[0];
                    OBS[1][i] = hhhhh[1];
                    OBS[2][i] = hhhhh[2];

                }

                getline(LUFOBS, PARAM,'\n');
                for(i=13;i<NBOBSA;i++){
                    lixo = PARAM.substr (7 +(4 * (i-13)),4);
                    lixo.copy(hhhhh,3);
                     OBS[0][i] = hhhhh[0];
                     OBS[1][i] = hhhhh[1];
                     OBS[2][i] = hhhhh[2];


                }
              }

              for(i=0;i<NBOBSA;i++){

                if((cop1[0] == OBS[0][i]) && (cop1[1] == OBS[1][i]) && (cop1[2] == OBS[2][i])){
                   COLP1A = i ;

                }
                if((cop2[0] == OBS[0][i]) && (cop2[1] == OBS[1][i]) && (cop2[2] == OBS[2][i])){

                   COLP2A = i ;

                }

                if((coc1[0] == OBS[0][i]) && (coc1[1] == OBS[1][i]) && (coc1[2] == OBS[2][i])){
                    COLCA = i ;

                }
                if((coc2[0] == OBS[0][i]) && (coc2[1] == OBS[1][i]) && (coc2[2] == OBS[2][i])){
                     COLC2A = i ;

                }

              }
              int cod1=0,cod2=0;
              long double FREQ1,FREQ2;

              if(CODE == 'G'){

                  cod1 = int(cop1[1]) - 48;
                  cod2 = int(cop2[1]) - 48;

                  switch (cod1) {
                  case 1:
                      FREQ1 = 1575.42;
                      break;
                  case 2:
                      FREQ1 = 1227.60;
                      break;
                  case 5:
                      FREQ1 = 1176.45;
                      break;
                  default:
                      cout<< "Codigo 1 nao definido."<<endl;
                      exit(3);
                      break;
                  }

                  switch (cod2) {
                  case (1):
                      FREQ2 = 1575.42;
                      break;
                  case (2):
                      FREQ2 = 1227.60;
                      break;
                  case (5):
                      FREQ2 = 1176.45;
                      break;
                  default:
                      cout<< "Codigo 2 nao definido."<<endl;
                      exit(3);
                      break;
                  }
                  FREQ1_FINAL = (FREQ1*FREQ1)/((FREQ1*FREQ1)-(FREQ2*FREQ2));
                  FREQ2_FINAL = (FREQ2*FREQ2)/((FREQ1*FREQ1)-(FREQ2*FREQ2));
              };

              if(CODE == 'E'){
                  cod1 = int(cop1[1]) - 48;
                  cod2 = int(cop2[1]) - 48;


                  switch (cod1) {
                  case 1:
                      FREQ1 = 1575.42;
                      break;
                  case 5:
                      FREQ1 = 1176.45;
                      break;
                  case 7:
                      FREQ1 = 1207.14;
                      break;
                  case 8:
                      FREQ1 = 1191.795;
                      break;
                  case 6:
                      FREQ1 = 1278.75;
                      break;

                  default:
                      cout<< "Codigo 1 nao definido."<<endl;
                      exit(3);
                      break;
                  }

                  switch (cod2) {
                  case 1:
                      FREQ2 = 1575.42;
                      break;
                  case 5:
                      FREQ2 = 1176.45;
                      break;
                  case 7:
                      FREQ2 = 1207.14;
                      break;
                  case 8:
                      FREQ2 = 1191.795;
                      break;
                  case 6:
                      FREQ2 = 1278.75;
                      break;

                  default:
                      cout<< "Codigo 1 nao definido."<<endl;
                      exit(3);
                      break;
                  }


                  FREQ1_FINAL = (FREQ1*FREQ1)/((FREQ1*FREQ1)-(FREQ2*FREQ2));
                  FREQ2_FINAL = (FREQ2*FREQ2)/((FREQ1*FREQ1)-(FREQ2*FREQ2));


              }



            }


        }


        else if((PARAM.compare(60,13, "END OF HEADER") == 0)&&(IFORMA >= 3)){

            goto aqui2;
        }
        else{};

    PARAM.clear();
    };
;
    aqui2:
    LUFOBP >> IFORMB;
    lixo2 = modf(IFORMB,&IFORMB);

    LUFOBP.clear();
    LUFOBP.seekg (0, ios::beg);


    while(getline(LUFOBP, PARAM,'\n')){

        if(PARAM.compare(60,19,"RCV CLOCK OFFS APPL")==0){

                   lixo = PARAM.substr (6,1);
                   istringstream (lixo) >> A;



               if(A==1){
                   OFSET = true;
               }

        }

        else if(PARAM.compare(61,17,"TIME OF FIRST OBS")==0){
            lixo = PARAM.substr (48,3);
            lixo.copy(TSCALE,3);

        }
        else if(PARAM.compare(60,19,"SYS / # / OBS TYPES")==0){
            string  constellationID;
            lixo = PARAM.substr (0,1);
            constellationID = lixo;

             if((constellationID.compare(0,1,&CODE)==0))
           {
             lixo = PARAM.substr (4,2);
             istringstream (lixo) >> NBOBSB;
              if(NBOBSB<13){
                for(i=0;i<NBOBSB;i++){
                    lixo = PARAM.substr (7 +(4 * i),4);
                    lixo.copy(hhhhh,4);
                    OBS[0][i] = hhhhh[0];
                    OBS[1][i] = hhhhh[1];
                    OBS[2][i] = hhhhh[2];

                }
              }

              if(NBOBSB>13){
                for(i=0;i<13;i++){
                    lixo = PARAM.substr (7 +(4 * i),4);
                    lixo.copy(hhhhh,3);
                    OBS[0][i] = hhhhh[0];
                    OBS[1][i] = hhhhh[1];
                    OBS[2][i] = hhhhh[2];

                }

                getline(LUFOBS, PARAM,'\n');
                for(i=13;i<NBOBSB;i++){
                    lixo = PARAM.substr (7 +(4 * (i-13)),4);
                    lixo.copy(hhhhh,3);
                     OBS[0][i] = hhhhh[0];
                     OBS[1][i] = hhhhh[1];
                     OBS[2][i] = hhhhh[2];
                }
              }

              for(i=0;i<NBOBSB;i++){
                if((cop1[0] == OBS[0][i]) && (cop1[1] == OBS[1][i]) && (cop1[2] == OBS[2][i])){
                   COLP1B = i ;

                }
                if((cop2[0] == OBS[0][i]) && (cop2[1] == OBS[1][i]) && (cop2[2] == OBS[2][i])){

                   COLP2B = i ;
                }

                if((coc1[0] == OBS[0][i]) && (coc1[1] == OBS[1][i]) && (coc1[2] == OBS[2][i])){
                    COLCAB = i ;
                }
                if((coc2[0] == OBS[0][i]) && (coc2[1] == OBS[1][i]) && (coc2[2] == OBS[2][i])){
                     COLC2B = i ;
                }

              }

            }
        }

        else if((PARAM.compare(60,13, "END OF HEADER") == 0) && (IFORMB >= 3)){

            goto fora;
        }
        else{};
    };

}



fora:
int IPASS = 0, IPASSV = 0,IPASS2 = 0;;

if(TSCALE == "   "){
    if(FTYPE == 'M'){
        cout<<"Time system not defined, GPS used by default"<<'\n';
        TSCALE[0] = 'G';TSCALE[1] = 'P';TSCALE[2] = 'S';
    };
    if(FTYPE == 'R'){
        cout<<"Time system not defined, GPS used by default"<<'\n';
       TSCALE[0] = 'G';TSCALE[1] = 'L';TSCALE[2] = 'O';
    };
    if(FTYPE == 'G'){
            cout<<"Time system not defined, GPS used by default"<<'\n';
           TSCALE[0] = 'G';TSCALE[1] = 'P';TSCALE[2] = 'S';
    };
};


LAST = MJD + 1 - 0.000347222222222222235;  //0.000347222 = 30/86400

DS = (0.1/86400);


char dateC1P1[20], lengC1P1[7];

int CK = 0, ICK = 0;

header(SVERSION, ITESTC1, lengC1P1, dateC1P1, REVDATE, FLOUT, RCVR, CH, LAB, ANTPOS, COMMENTS, RECDELP1, RECDELP2, CABDEL, REFDEL, REF, CK, ICK,cop1,cop2);

LUOUT.open (FLOUT);
if (LUOUT.good()){}
   else{
      cout << "WARNING:" << '\n';
      cout << "fail to open file unit " <<  FLOUT << '\n';
       };

LUOUT.seekg (0, ios::end);



//********************************* INICIO DOS CALCULOS ****************************************



int ISCH=0, KSEC = 0, IPK=0, ISAT=0, NN = 36, TRKL[NN], IC[NN], KKK=0, IV, LFLAG, YY, MM, DD, HH, MIN, NNSAT;
int SATNUM[NN], NLINE, iii = 0, OBSERV = 18, COL1GPS=(-1), COL2GPS=(-1), SATN[NN], IOE;
int aux5=0;
int icoP1g, icoP2g, icoC1g, icoC2g, icoP1r, icoP2r, icoC1r, icoC2r, COL1, COL2, NBSAT, MJDTRK;
long double CLKOFFSET=0, TIMEFIRST=0, MEASIONO[28][NN], RESQUAD[28][NN], MOMENT=0, TIMESTOP=0, SEC, DELTATK;
long double  IDATE, VALOBS[OBSERV][NN], PC1[NN], PC2[NN], DELTAT[28], P1VALOBS[NN], P2VALOBS[NN], TOC;
long double EPOCH[28], DJUL, CORPREV, TRC, TAU, CORRGEOM, TTR, TREL, X[3], ALPHA, XS[3], IONO[28],hhh;
long double EL, AZ, REFSV[28], REFSVMIL, SRSV, STDV, REFGPS[28], REFGPSMIL,SRGPS, DSG, TROPO[28], MDTR, SMDT, SDTV, MDIO, SMDI, ISG;
long double  LLA2[3], MF, ZPD, PCOR, CR, TCLOCK, CLOCKSAT, TGD, SECSCH;
string strin,ajuda;
char SATGROUP[NN], FRC1GPS[3], FRC2GPS[3], FR1[3][NN], FR2[3][NN], FF1[3][NN], FF2[3][NN], CLASS[2], FRC1[3], FRC2[3] ;
bool stat;
double AZTH, ELV;


IV = 0;

for(ISCH = 0; ISCH < NSCH; ISCH++){

    CLKOFFSET = 0;
    IPK = 0;
    KSEC = 0;
    TIMEFIRST = SCHTIME[ISCH];

    for(ISAT = 0; ISAT < NN; ISAT++){
        TRKL[ISAT] = 0;
        IC[ISAT] = 0;
        for(KKK = 0; KKK < (MINUTOS_VETOR * 2); KKK++){
            MEASIONO[KKK][ISAT] = 0;
            RESQUAD[KKK][ISAT] = 0;
         }

    }
    MOMENT = 0;
    hhh = (((MINUTOS_VETOR/24.0)/60.0 )-1.0/86400);      //0.00901620370132150128;

    TIMESTOP = SCHTIME[ISCH] + hhh; //0.00901620370132150128;


    novamente:
    if((IV == 1)&&(LUFOBP.good())){   // Lendo os dados de época do arquivo de observação do segundo dia

        strin.clear();

        getline(LUFOBP,strin,'\n');    // Lendo arquivo de observação do segundo dia, logo depois do 'END OF HEADER'


       if(strin.substr(0,1).compare(0,1,">")==0) // Verifica se é uma linha com informações de época
       {

         if(strin.size()>=35) // Verifica se o tamanho da string é maior ou igual a 35 para não dar crash
         {
           ajuda = strin.substr (31,1);
           istringstream (ajuda) >> LFLAG; // 0 Ok; 1 Power failure between previous and current epoch; >1 special event
           if((LFLAG == 0)||(LFLAG == 1))
            {
             ajuda = strin.substr (2,4);
             istringstream (ajuda) >> YY;
             ajuda = strin.substr (7,2);
             istringstream (ajuda) >> MM;
             ajuda = strin.substr (10,2);
             istringstream (ajuda) >> DD;
             ajuda = strin.substr (13,2);
             istringstream (ajuda) >> HH;
             ajuda = strin.substr (16,2);
             istringstream (ajuda) >> MIN;
             ajuda = strin.substr (18,11);
             istringstream (ajuda) >> SEC;
             ajuda = strin.substr (32,3);
             istringstream (ajuda) >> NNSAT;

             if(strin.size()>=57) // Se houver dados do offset do relógio do receptor o tamanho da linha deverá ser 57
             {
                 ajuda = strin.substr (41,15);
                 istringstream (ajuda) >> CLKOFFSET;
             }

           }
           else if(LFLAG == 4)
           {

                 ajuda = strin.substr (25,3);
                 istringstream (ajuda) >> LFLAG;
                 ajuda = strin.substr (28,3);
                 istringstream (ajuda) >> NLINE;
                 for (i=1; i<NLINE; i++)
                 {
                     LUFOBS.ignore(81,'\n');
                 }


               goto novamente;
            }
             else  if(LFLAG == 2)
            {
                 cout << "Moving antenna--stop"<<'\n';
                 exit(1);
            };

          // Pega os dados dos satélites
       }
      }
    }
    else if(IV == 0)
    {                       // O programa entra aqui assim que inicia a leitura dos dados de observação
         strin.clear();
         getline(LUFOBS,strin,'\n');    // Lendo arquivo de observação do primeiro dia, logo depois do 'END OF HEADER'


        if(strin.substr(0,1).compare(0,1,">")==0) // Verifica se é uma linha com informações de época
        {

          if(strin.size()>=35) // Verifica se o tamanho da string é maior ou igual a 35 para não dar crash
          {
            ajuda = strin.substr (31,1);
            istringstream (ajuda) >> LFLAG; // 0 Ok; 1 Power failure between previous and current epoch; >1 special event
            if((LFLAG == 0)||(LFLAG == 1))
             {
              ajuda = strin.substr (2,4);
              istringstream (ajuda) >> YY;
              ajuda = strin.substr (7,2);
              istringstream (ajuda) >> MM;
              ajuda = strin.substr (10,2);
              istringstream (ajuda) >> DD;
              ajuda = strin.substr (13,2);
              istringstream (ajuda) >> HH;
              ajuda = strin.substr (16,2);
              istringstream (ajuda) >> MIN;
              ajuda = strin.substr (18,11);
              istringstream (ajuda) >> SEC;
              ajuda = strin.substr (32,3);
              istringstream (ajuda) >> NNSAT;



              if(strin.size()>=57) // Se houver dados do offset do relógio do receptor o tamanho da linha deverá ser 57
              {
                  ajuda = strin.substr (41,15);
                  istringstream (ajuda) >> CLKOFFSET;
              }
              if(LFLAG == 1) // Power failure between previous and current epoch
                          {
                              cout << "Power failure (lflag=1) -- skip"<<'\n';
                          }
             }
            else if(LFLAG == 4)
            {

                  ajuda = strin.substr (25,3);
                  istringstream (ajuda) >> LFLAG;
                  ajuda = strin.substr (28,3);
                  istringstream (ajuda) >> NLINE;
                  for (i=1; i<NLINE; i++)
                  {
                      LUFOBS.ignore(81,'\n');
                  }

                goto novamente;     // Depois, acabar de vez com o "gotos" do programa!!!
             }
              else  if(LFLAG == 2)
             {
                  cout << "Moving antenna--stop"<<'\n';
                  exit(1);
             };
           }
        }
       };
    j = 0;

    DELTATK = 0;

    if(fabs(SEC - 60) <= 0.003){
        DELTATK = 60 - SEC;
    };
    if(fabs(SEC - 30) <= 0.003){
        DELTATK = 30 - SEC;
    };
    if(fabs(SEC) == 0.003){
        DELTATK = - SEC;
    };

    SEC = SEC + DELTATK;

    if(OFSET == true){
        DELTATK = - CLKOFFSET;
    };

    if((TSCALE[0] != 'G')||(TSCALE[1] != 'P')||(TSCALE[2] != 'S')){

        if(LEAPSEC != 0){
            leapscorr(DD,HH, MIN, SEC, LEAPSEC);

        };
    };

    datemjd(YY, MM, DD, HH, MIN, SEC, MOMENT,0, CODE);


    if((IV == 1) && (IPASSV == 0) && (LUFOBP.good())){
        IPASSV = 1;
        auxiliar = (MOMENT - MJD - 1.0);
        lixo2 = modf(auxiliar, &IDATE);

        if(IDATE != 0){
            cout << "Please verify the dates in the file 'rinex_obs_p'" << '\n';
            exit(1);
        };
    }
    if((IV == 0) && (IPASS == 0)){
        IPASS = 1;
        IPASS2 = 1;
        auxiliar = (MOMENT - MJD);
        lixo2 = modf(auxiliar, &IDATE);

        if(IDATE != 0){
            cout << "Please verify the dates in the file 'rinex_obs'" << '\n';
            exit(1);
        };
    };

 //           read observations of the epoch
    for(iii=0;iii<NNSAT;iii++)
    { if(IV == 0){
        strin.clear();
        getline(LUFOBS,strin,'\n');

        }
        else if(IV == 1){
            strin.clear();
            getline(LUFOBP,strin,'\n');

        }

      char c=strin.at(0);
      SATGROUP[iii]= c;
      SATNUM[iii]= atoi(strin.substr(1,2).c_str());


      // Se for GLONASS acrescenta 100 ao SATNUM[i]
      if(SATGROUP[iii] == 'R')
      {
          SATNUM[iii] = SATNUM[iii] + 100;
      };
      // Se for BEIDOU acrescenta 200 ao SATNUM[i]
      if(SATGROUP[iii] == 'C')
      {

          SATNUM[iii] = SATNUM[iii] + 200;
      };
      // Se for GALILEO acrescenta 300 ao SATNUM[i]
      if(SATGROUP[iii] == 'E')
      {
          SATNUM[iii] = SATNUM[iii] + 300;
      };


       // Pega dados dos satélites
        if((IV == 0) && (LUFOBP.good())){


           for(k=0; k<NBOBSA; k++)
           {
              VALOBS[k][iii] = 0;
              if(strin.length()>(3 + (16 * k) + 14))
              {
                ajuda = strin.substr (3 +(16 * k), 14);

                if((ajuda != "              ")&&(ajuda !="")){
                   istringstream (ajuda) >> VALOBS[k][iii];


                 };

              }

           };


          }
          else if( IV == 1 ){
            for(k=0; k<NBOBSB; k++)
            {
               VALOBS[k][iii] = 0;
               if(strin.length()>(3+ (16 * k) + 14))
               {
                 ajuda = strin.substr (3+(16 * k),14);
                 if((ajuda != "              ")&&(ajuda !="")){
                    istringstream (ajuda) >> VALOBS[k][iii];

                  };
               }

            };


     }
    }


  if((IPASS == 1)||(IPASS2 == 1)){
      icoP1g=0;
      icoP2g=0;
      icoC1g=0;
      icoC2g=0;
      icoP1r=0;
      icoP2r=0;
      icoC1r=0;
      icoC2r=0;
      for(iii = 0; iii < NNSAT; iii++){
        if(CODE == 'G'){
            if( SATNUM[iii] < 100 ){

                if(VALOBS[COLP1A][iii] != 0){
                    icoP1g = icoP1g + 1;
                    IPASS = 3;

                }
                if(VALOBS[COLCA][iii] != 0){
                    icoC1g = icoC1g + 1;
                    IPASS2 = 3;
                }
                if(VALOBS[COLP2A][iii] != 0){

                    icoP2g = icoP2g + 1;
                    IPASS = 3;
                }

                if(VALOBS[COLC2A][iii] != 0){

                    icoC2g = icoC2g + 1;
                    IPASS2 = 3;
                }

            };
        }
        else if(CODE == 'C'){
            if(( SATNUM[iii] < 300 )&&( SATNUM[iii] > 200 )){

                if(VALOBS[COLP1A][iii] != 0){
                    icoP1g = icoP1g + 1;
                    IPASS = 3;


                }
                if(VALOBS[COLCA][iii] != 0){
                    icoC1g = icoC1g + 1;
                    IPASS2 = 3;
                }
                if(VALOBS[COLP2A][iii] != 0){

                    icoP2g = icoP2g + 1;
                    IPASS = 3;
                }

                if(VALOBS[COLC2A][iii] != 0){

                    icoC2g = icoC2g + 1;
                    IPASS2 = 3;
                }

            };
        }
        else if(CODE == 'E'){
            if(( SATNUM[iii] > 300 ) &&( SATNUM[iii] < 400)){

                if(VALOBS[COLP1A][iii] != 0){
                    icoP1g = icoP1g + 1;
                    IPASS = 3;


                }
                if(VALOBS[COLCA][iii] != 0){
                    icoC1g = icoC1g + 1;
                    IPASS2 = 3;
                }
                if(VALOBS[COLP2A][iii] != 0){

                    icoP2g = icoP2g + 1;
                    IPASS = 3;
                }

                if(VALOBS[COLC2A][iii] != 0){

                    icoC2g = icoC2g + 1;
                    IPASS2 = 3;
                }



            };
        }
        if(icoP1g != 0){
          FRC1GPS[0] = 'L';
          FRC1GPS[1] = '1';
          FRC1GPS[2] = 'P';
        };
        if((icoP1g == 0) && (icoC1g != 0)){
            FRC1GPS[0] = 'L';
            FRC1GPS[1] = '1';
            FRC1GPS[2] = 'C';
        };
        if(icoP2g != 0){
            FRC2GPS[0] = 'L';
            FRC2GPS[1] = '2';
            FRC2GPS[2] = 'P';
        };
        if((icoP2g == 0) && (icoC2g != 0)){
            FRC2GPS[0] = 'L';
            FRC2GPS[1] = '2';
            FRC2GPS[2] = 'C';
        };
      };


  };

//     defining the corresponding column:
  if(IV == 1){

    if((FRC1GPS[0]=='L') && (FRC1GPS[1]=='1') && (FRC1GPS[2]=='P')){
        COL1GPS = COLP1B;

    };
    if((FRC1GPS[0]=='L') && (FRC1GPS[1]=='1') && (FRC1GPS[2]=='C')){
        COL1GPS = COLCAB;

    };
    if((FRC2GPS[0]=='L') && (FRC2GPS[1]=='2') && (FRC2GPS[2]=='P')){
        COL2GPS = COLP2B;

    };
    if((FRC2GPS[0]=='L') && (FRC2GPS[1]=='2') && (FRC2GPS[2]=='C')){
        COL2GPS = COLC2B;

    };

  }
  else{

      if((FRC1GPS[0]=='L') && (FRC1GPS[1]=='1') && (FRC1GPS[2]=='P')){
          COL1GPS = COLP1A;

      };
      if((FRC1GPS[0]=='L') && (FRC1GPS[1]=='1') && (FRC1GPS[2]=='C')){
          COL1GPS = COLCA;

      };
      if((FRC2GPS[0]=='L') && (FRC2GPS[1]=='2') && (FRC2GPS[2]=='P')){
          COL2GPS = COLP2A;

      };
      if((FRC2GPS[0]=='L') && (FRC2GPS[1]=='2') && (FRC2GPS[2]=='C')){
          COL2GPS = COLC2A;
         ;
      };




  };


//    put the right observations in valobs for use in P3

  for(iii = 0; iii < NNSAT; iii++){
    PC1[iii] = 0;
    PC2[iii] = 0;
    if(((SATNUM[iii] < 100)&&(CODE=='G'))||((CODE=='C')&&(( SATNUM[iii] < 300 )&&( SATNUM[iii] > 200 )))||((CODE  == 'E') && ( SATNUM[iii] > 300 ) &&( SATNUM[iii] < 400))){

        if((COL1GPS != -1) && (COL2GPS != -1)){

            PC1[iii] = VALOBS[COL1GPS][iii];
            PC2[iii] = VALOBS[COL2GPS][iii];

            FR1[0][iii] = FRC1GPS[0];
            FR1[1][iii] = FRC1GPS[1];
            FR1[2][iii] = FRC1GPS[2];
            FR2[0][iii] = FRC2GPS[0];
            FR2[1][iii] = FRC2GPS[1];
            FR2[2][iii] = FRC2GPS[2];
        };
     };
  };
  if(CODE=='C'){
      auxiliar = fabs(MOMENT - LAST);
  }
  else{
    auxiliar = fabs(MOMENT - LAST);
  }



  if(CODE == 'C'){
    if((IV == 0) && (fabs(MOMENT - LAST + 0.000162037037032547) < (0.00000115740740740740739))){

              IV = 1;
    };
  }
  else{
      if((IV == 0) && (fabs(MOMENT - LAST) < (0.00000115740740740740739))){

                IV = 1;
      };
  };
    auxiliar = fabs(SEC - 60);

  if( (auxiliar >= 0.004) && (fabs(SEC - 30) >= 0.004) && (fabs(SEC) >= 0.004)){
      goto novamente;
  };

//    construction of the 13 min vectors

    auxiliar = TIMESTOP + DS;
    if((KSEC == 0) && (MOMENT > auxiliar)){
        goto again;
    };

    if((MOMENT >= (TIMEFIRST - DS)) && (MOMENT <= (TIMESTOP + DS))){

        DELTAT[KSEC] = DELTATK;

        if(CLKOFFSET != 0){
            DELTAT[KSEC] = - CLKOFFSET;
        }


        COL1 = 1;
        COL2 = 2;
        if(IPK == 1){
            for(j=0; j<25; j++){
                P1VALOBS[j] = 0;
                P2VALOBS[j] = 0;
                FF1[0][j] = '0';
                FF1[1][j] = '0';
                FF1[2][j] = '0';
                FF2[0][j] = '0';
                FF2[1][j] = '0';
                FF2[2][j] = '0';
            };
            for(i=0; i<NNSAT; i++){

                for(j=0; j<NBSAT; j++){

                    if(SATNUM[i] == SATN[j]){

                        P1VALOBS[j] = PC1[i];
                        P2VALOBS[j] = PC2[i];

                        FF1[0][j] = FR1[0][i];
                        FF1[1][j] = FR1[1][i];
                        FF1[2][j] = FR1[2][i];
                        FF2[0][j] = FR2[0][i];
                        FF2[1][j] = FR2[1][i];
                        FF2[2][j] = FR2[2][i];
                    };
                };
            };

            for(j=0; j<NBSAT; j++){
                VALOBS[COL1][j] = P1VALOBS[j];
                VALOBS[COL2][j] = P2VALOBS[j];

            };
        };

        if(IPK == 0){
            NBSAT = NNSAT;
            for(i=0; i<NBSAT; i++){
                SATN[i] = SATNUM[i];
                VALOBS[COL1][i] = PC1[i];
                VALOBS[COL2][i] = PC2[i];

                FF1[0][i] = FR1[0][i];
                FF1[1][i] = FR1[1][i];
                FF1[2][i] = FR1[2][i];
                FF2[0][i] = FR2[0][i];
                FF2[1][i] = FR2[1][i];
                FF2[2][i] = FR2[2][i];
             };

            IPK = 1;
        };

//     construction of the vectors

        EPOCH[KSEC] = MOMENT;

        for(i=0; i<NBSAT; i++){

            if(((SATN[i]<100)&&(CODE=='G')) || ((CODE=='C')&&((SATN[i]<300)&&(SATN[i]>200))) || ((CODE  == 'E') && ( SATNUM[i] > 300 ) &&( SATNUM[i] < 400))){

                if((VALOBS[COL1][i] != 0) && (VALOBS[COL2][i] != 0)){
                    if(COLP1A == (-1)){
                        if(BIAS[SATN[i]] != 99){
                            VALOBS[COL1][i] = VALOBS[COL1][i] + (BIAS[SATN[i]] * VITLUM * 0.000000001);
                        }
                        else{
                            VALOBS[COL1][i] = 0;
                        };

                    };
                    if(COLP2A == (-1)){

                        if( BIAS2[SATN[i]] != 99){
                            VALOBS[COL2][i] = VALOBS[COL2][i] + (BIAS2[SATN[i]] * VITLUM * 0.000000001);
                        }
                        else{
                            VALOBS[COL2][i] = 0;
                        };
                    };
                };
             };

           if((VALOBS[COL1][i] != 0) && (VALOBS[COL2][i] != 0)){

               if((CODE == 'G')|| (CODE == 'E')){
                    RESQUAD[KSEC][i] = (FREQ1_FINAL * (VALOBS[COL1][i] - RECDELP1)) - (FREQ2_FINAL * (VALOBS[COL2][i] - RECDELP2));

               }
               else if(CODE == 'C'){

                   RESQUAD[KSEC][i] = (2.483848380914148 * (VALOBS[COL1][i] - RECDELP1)) - (1.483848380914147 * (VALOBS[COL2][i] - RECDELP2));
               }

               MEASIONO[KSEC][i] = (VALOBS[COL1][i] - RECDELP1) - RESQUAD[KSEC][i];
               TRKL[i] = TRKL[i] + 1;

               IC[i] = IC[i] + 1;


           };
        };
    KSEC++;
    };


    if(MOMENT < (TIMESTOP - DS)){
        goto novamente;
    };


    for(ISAT = 0; ISAT < NBSAT; ISAT++){

        if((CODE == 'G')){

            if((TRKL[ISAT] == (MINUTOS_VETOR*2)) && (SATN[ISAT] < 100)){
                jkl = 0;
                while(jkl < 3000){

                    //0,08333 = 2/24

                    if((PPRN[jkl] == SATN[ISAT]) && (SCHTIME[ISCH] <= DATENAV[jkl]) && ((DATENAV[jkl] - SCHTIME[ISCH]) <= (0.08333))){
                        DJUL = DATENAV[jkl] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TOC = (DJUL * 86400);

                        goto pulo;
                    };
                    jkl++;
                };
                if(jkl == 3000){
                    goto outravez;
                };
                // reject unhealthy satellites
                pulo:
                if(BRORB[jkl][5][1] != 0){

                    goto outravez;
                };

                for(KSEC = 0; KSEC < (MINUTOS_VETOR*2); KSEC++){
                    if(RESQUAD[KSEC][ISAT] != 0){
                        iii = 0;
                        CORPREV = 0;
                        DJUL = EPOCH[KSEC] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TRC = DJUL * 86400;
                        IOE = BRORB[jkl][0][0];

                        pont_iii:

                        if(iii == 0){
                            TAU = ((RESQUAD[KSEC][ISAT]) / VITLUM);
                        };
                        if(iii == 1){
                            TAU = (CORRGEOM / VITLUM);
                        };
                        aux5 = llround(TRC);
                        TTR = aux5 - TAU;

                        satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                        ALPHA = TAU * WE;

                        XS[0] = (X[0] * cos(ALPHA)) + (X[1] * sin(ALPHA));
                        XS[1] = (-X[0] * sin(ALPHA)) + (X[1] * cos(ALPHA));
                        XS[2] = X[2];

                        CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2]) * (ANTPOS[2] - XS[2])));

                        auxiliar = fabs(CORRGEOM - CORPREV);

                        if(auxiliar >= 0.00001){
                            CORPREV = CORRGEOM;
                            iii = 1;
                            goto pont_iii;
                        };


                        IONO[KSEC] = MEASIONO[KSEC][ISAT];

                        azel(XS, ANTPOS, AZ, EL);


                        xyzlla(ANTPOS, LLA2);

                        MF = 1.0/(sin(EL) + (0.00143/ (tan(EL) + 0.0455)));


                        if(LLA2[2] <= 1000){
                            ZPD = 2162.0 + (NS * (1.0 - (LLA2[2] / 1000.0))) + (0.5 * DeltaN * (1.0 - (LLA2[2]/1000.0) * (LLA2[2]/1000.0)));
                        }
                        else{
                            ZPD = 732.0 - 8.0 * (NS * DeltaN)/ NSLOG * (exp(-NSLOG) - exp(0.125*(1.0 - (LLA2[2])/1000.0) * NSLOG));
                        };
                        TROPO[KSEC] = MF * ZPD/1000;

                        PCOR = RESQUAD[KSEC][ISAT] + (TREL * VITLUM) - TROPO[KSEC];

                        CR = PCOR - CORRGEOM;



                        if(OFSET == 0){
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL;

                        }
                        else{
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL + (DELTAT[KSEC] * auxiliar);

                        };

                        TCLOCK = TTR - TOC;

                        if(TCLOCK > 302400){
                            TCLOCK = TCLOCK - 604800;
                        };
                        if(TCLOCK < -302400){
                            TCLOCK = TCLOCK + 604800;
                        };

                        auxiliar = pow(10,10);
                        CLOCKSAT = (SVCLK[jkl][0] + (SVCLK[jkl][1] * TCLOCK) + (SVCLK[jkl][2] * TCLOCK * TCLOCK)) * auxiliar;

                        //  computation of  REFGPS (unit= 0.1 ns)
                        REFGPS[KSEC] = REFSV[KSEC] + CLOCKSAT;

                    }
                    else{

                        REFGPS[KSEC] = 0;
                        REFSV[KSEC] = 0;
                        TROPO[KSEC] = 0;
                        IONO[KSEC] = 0;

                    };

                };

                TGD = BRORB[jkl][5][2];

                // AZth et ELv at the midpoint of the track

                CORPREV = 0;
                iii = 0;
                //0.004513889 = 6.5/24/60
                auxiliar = LEAPSEC / 86400.0;
                DJUL = SCHTIME[ISCH] - (ILEAPS/86400) + (((MINUTOS_VETOR/2.0)/24.0)/60.0) - 44244.0 + (auxiliar);


                DJUL = fmod(DJUL , 7);

                TRC = llround(DJUL * 86400.0);


                LLL:
                if(iii == 0){
                    TAU = RESQUAD[12][ISAT] / VITLUM;
                };

                if(iii == 1){
                    TAU = CORRGEOM / VITLUM;
                };
                aux5 = llround(TRC);
                TTR = aux5  - TAU;


                satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                ALPHA =  TAU * WE;
                XS[0] = X[0] * cos(ALPHA) + X[1] * sin(ALPHA);
                XS[1] = - X[0] * sin(ALPHA) + X[1] * cos(ALPHA);
                XS[2] = X[2];
                CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2])*(ANTPOS[2] - XS[2])) );

                auxiliar = fabs(CORRGEOM - CORPREV);

                if(auxiliar >= 0.00001){

                    CORPREV = CORRGEOM;
                    iii = 1;
                    goto LLL;
                };

               azel(XS, ANTPOS, AZ, EL);

                EL = (EL/pi *1800);
                AZ = (AZ/pi *1800);

                ELV = llround(EL);
                AZTH = llround(AZ);


                fitlin (EPOCH, REFSV, REFSVMIL, SRSV, STDV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, REFGPS, REFGPSMIL, SRGPS, DSG, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, TROPO, MDTR, SMDT, SDTV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, IONO, MDIO, SMDI, ISG, LEAPSEC, MINUTOS_VETOR);

                // iono and tropo in 0.1 ns
                auxiliar = pow(10,10);
                MDTR = MDTR / VITLUM * auxiliar;
                MDIO = (MDIO/ VITLUM - TGD)* auxiliar;
                ISG = ISG / VITLUM * auxiliar;
                SRSV = SRSV * 1000 / 86400;
                SRGPS = SRGPS * 1000 / 86400;
                SMDT = SMDT / VITLUM * auxiliar * 1000 / 86400;
                SMDI = SMDI / VITLUM * auxiliar * 1000 / 86400;

                CLASS[0] = 'F';
                CLASS[1] = 'F';
                FRC1[0] = FF1[0][ISAT];
                FRC1[1] = FF1[1][ISAT];
                FRC1[2] = FF1[2][ISAT];
                FRC2[0] = FF2[0][ISAT];
                FRC2[1] = FF2[1][ISAT];
                FRC2[2] = FF2[2][ISAT];

                TRKL[ISAT] = TRKL[ISAT] * 30;

                SECSCH = 0;
                MJDTRK = MJD;
                ICK = 0;
                if((SCHMIN[ISCH] > (61-MINUTOS_VETOR)) && (SCHH[ISCH] > 23)){
                    MJDTRK = MJD - 1;

                }

                ostringstream converter99,converter100,converter101,converter98;
                char CONVER_CK[135];
                string abcd;

                converter98.str("");
                converter98 << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;
                LUOUT << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;

                NAME.clear();

                if(SCHH[ISCH]<10){
                    NAME = '0';
                }
                converter99<< dec  << SCHH[ISCH];
                NAME = NAME + converter99.str();
                if(SCHMIN[ISCH]<10){
                    NAME = NAME + '0';
                }
                converter100<< dec  << SCHMIN[ISCH];
                NAME = NAME + converter100.str();
                if(SECSCH<10){
                    NAME = NAME + '0';
                }
                converter101 << dec << lround(SECSCH);
                NAME = NAME + converter101.str();

                converter98 << NAME;

                converter98.fill(' ');
                converter98 << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                converter98 << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                converter98 << setw(6)  << dec              << lround(SRSV)        << " ";
                converter98 << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                converter98 << setw(6)  << dec              << lround(SRGPS)       << " ";

                converter98 << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                converter98 << setw(4)  << dec              << lround(MDTR)        << " ";

                if(abs(SMDT) >= 999){
                    converter98 << setw(4)<<right << "****"<< " ";
                }
                else{
                    converter98 << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }
                converter98 << setw(4)  << dec              << lround(MDIO)        << " ";
                converter98 << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                converter98 << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                converter98 << setw(4) << lround(SMDI)        << " ";
                converter98 << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " ";
                abcd.clear();
                abcd = converter98.str();
                abcd.copy(CONVER_CK,abcd.length());
                CK = 0;
                for(i=0;i<125;i++){
                    CK = CK + CONVER_CK[i];
                }
                CK = (CK % 256);

                LUOUT <<NAME;

                LUOUT.fill(' ');
                LUOUT << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                LUOUT << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                LUOUT << setw(6)  << dec              << lround(SRSV)        << " ";
                LUOUT << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                LUOUT << setw(6)  << dec              << lround(SRGPS)       << " ";
                LUOUT << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                if(abs(MDTR) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDTR)        << " ";
                }

                if(abs(SMDT) > 999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDIO)        << " ";
                }

                if(abs(SMDI) > 999){
                     LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                }


                LUOUT << setw(4) << lround(SMDI)        << " ";
                LUOUT << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " " << setw(2) << hex <<uppercase<< CK << " " << FRC1[0] << FRC1[1] << FRC1[2] << " " << FRC2[0] << FRC2[1] << FRC2[2] << " "<<endl;

            };
        }
        if((CODE == 'E')){

            if((TRKL[ISAT] == (MINUTOS_VETOR*2)) && (SATN[ISAT] < 400)&&(SATN[ISAT] > 300)){
                jkl = 0;
                while(jkl < 3000){

                    //0,08333 = 2/24

                    if((PPRN[jkl] == (SATN[ISAT]-300)) && (SCHTIME[ISCH] <= DATENAV[jkl]) && ((DATENAV[jkl] - SCHTIME[ISCH]) <= (0.08333))){
                        DJUL = DATENAV[jkl] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TOC = (DJUL * 86400);

                        goto pulo_gali;
                    };
                    jkl++;
                };
                if(jkl == 3000){
                    goto outravez;
                };
                // reject unhealthy satellites
                pulo_gali:
                if(BRORB[jkl][5][1] != 0){

                    goto outravez;
                };

                for(KSEC = 0; KSEC < (MINUTOS_VETOR*2); KSEC++){
                    if(RESQUAD[KSEC][ISAT] != 0){
                        iii = 0;
                        CORPREV = 0;
                        DJUL = EPOCH[KSEC] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TRC = DJUL * 86400;
                        IOE = BRORB[jkl][0][0];

                        pont_iii_gali:

                        if(iii == 0){
                            TAU = ((RESQUAD[KSEC][ISAT]) / VITLUM);
                        };
                        if(iii == 1){
                            TAU = (CORRGEOM / VITLUM);
                        };
                        aux5 = llround(TRC);
                        TTR = aux5 - TAU;

                        satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                        ALPHA = TAU * WE;

                        XS[0] = (X[0] * cos(ALPHA)) + (X[1] * sin(ALPHA));
                        XS[1] = (-X[0] * sin(ALPHA)) + (X[1] * cos(ALPHA));
                        XS[2] = X[2];

                        CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2]) * (ANTPOS[2] - XS[2])));

                        auxiliar = fabs(CORRGEOM - CORPREV);

                        if(auxiliar >= 0.00001){
                            CORPREV = CORRGEOM;
                            iii = 1;
                            goto pont_iii_gali;
                        };


                        IONO[KSEC] = MEASIONO[KSEC][ISAT];

                        azel(XS, ANTPOS, AZ, EL);


                        xyzlla(ANTPOS, LLA2);

                        MF = 1.0/(sin(EL) + (0.00143/ (tan(EL) + 0.0455)));


                        if(LLA2[2] <= 1000){
                            ZPD = 2162.0 + (NS * (1.0 - (LLA2[2] / 1000.0))) + (0.5 * DeltaN * (1.0 - (LLA2[2]/1000.0) * (LLA2[2]/1000.0)));
                        }
                        else{
                            ZPD = 732.0 - 8.0 * (NS * DeltaN)/ NSLOG * (exp(-NSLOG) - exp(0.125*(1.0 - (LLA2[2])/1000.0) * NSLOG));
                        };
                        TROPO[KSEC] = MF * ZPD/1000;

                        PCOR = RESQUAD[KSEC][ISAT] + (TREL * VITLUM) - TROPO[KSEC];

                        CR = PCOR - CORRGEOM;



                        if(OFSET == 0){
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL;

                        }
                        else{
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL + (DELTAT[KSEC] * auxiliar);

                        };

                        TCLOCK = TTR - TOC;

                        if(TCLOCK > 302400){
                            TCLOCK = TCLOCK - 604800;
                        };
                        if(TCLOCK < -302400){
                            TCLOCK = TCLOCK + 604800;
                        };

                        auxiliar = pow(10,10);
                        CLOCKSAT = (SVCLK[jkl][0] + (SVCLK[jkl][1] * TCLOCK) + (SVCLK[jkl][2] * TCLOCK * TCLOCK)) * auxiliar;

                        //  computation of  REFGPS (unit= 0.1 ns)
                        REFGPS[KSEC] = REFSV[KSEC] + CLOCKSAT;

                    }
                    else{

                        REFGPS[KSEC] = 0;
                        REFSV[KSEC] = 0;
                        TROPO[KSEC] = 0;
                        IONO[KSEC] = 0;

                    };

                };

                TGD = BRORB[jkl][5][2];

                // AZth et ELv at the midpoint of the track

                CORPREV = 0;
                iii = 0;
                //0.004513889 = 6.5/24/60
                auxiliar = LEAPSEC / 86400.0;
                DJUL = SCHTIME[ISCH] - (ILEAPS/86400) + (((MINUTOS_VETOR/2.0)/24.0)/60.0) - 44244.0 + (auxiliar);


                DJUL = fmod(DJUL , 7);

                TRC = llround(DJUL * 86400.0);


                LLL_gali:
                if(iii == 0){
                    TAU = RESQUAD[12][ISAT] / VITLUM;
                };

                if(iii == 1){
                    TAU = CORRGEOM / VITLUM;
                };
                aux5 = llround(TRC);
                TTR = aux5  - TAU;


                satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                ALPHA =  TAU * WE;
                XS[0] = X[0] * cos(ALPHA) + X[1] * sin(ALPHA);
                XS[1] = - X[0] * sin(ALPHA) + X[1] * cos(ALPHA);
                XS[2] = X[2];
                CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2])*(ANTPOS[2] - XS[2])) );

                auxiliar = fabs(CORRGEOM - CORPREV);

                if(auxiliar >= 0.00001){

                    CORPREV = CORRGEOM;
                    iii = 1;
                    goto LLL_gali;
                };

                azel(XS, ANTPOS, AZ, EL);

                EL = (EL/pi *1800);
                AZ = (AZ/pi *1800);

                ELV = llround(EL);
                AZTH = llround(AZ);


                fitlin (EPOCH, REFSV, REFSVMIL, SRSV, STDV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, REFGPS, REFGPSMIL, SRGPS, DSG, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, TROPO, MDTR, SMDT, SDTV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, IONO, MDIO, SMDI, ISG, LEAPSEC, MINUTOS_VETOR);

                // iono and tropo in 0.1 ns
                auxiliar = pow(10,10);
                MDTR = MDTR / VITLUM * auxiliar;
                MDIO = (MDIO/ VITLUM - TGD)* auxiliar;
                ISG = ISG / VITLUM * auxiliar;
                SRSV = SRSV * 1000 / 86400;
                SRGPS = SRGPS * 1000 / 86400;
                SMDT = SMDT / VITLUM * auxiliar * 1000 / 86400;
                SMDI = SMDI / VITLUM * auxiliar * 1000 / 86400;

                CLASS[0] = 'F';
                CLASS[1] = 'F';
                FRC1[0] = FF1[0][ISAT];
                FRC1[1] = FF1[1][ISAT];
                FRC1[2] = FF1[2][ISAT];
                FRC2[0] = FF2[0][ISAT];
                FRC2[1] = FF2[1][ISAT];
                FRC2[2] = FF2[2][ISAT];

                TRKL[ISAT] = TRKL[ISAT] * 30;

                SECSCH = 0;
                MJDTRK = MJD;
                ICK = 0;
                if((SCHMIN[ISCH] > (61-MINUTOS_VETOR)) && (SCHH[ISCH] > 23)){
                    MJDTRK = MJD - 1;

                }

                ostringstream converter99,converter100,converter101,converter98;
                char CONVER_CK[135];
                string abcd;

                converter98.str("");
                converter98 << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;
                LUOUT << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;

                NAME.clear();

                if(SCHH[ISCH]<10){
                    NAME = '0';
                }
                converter99<< dec  << SCHH[ISCH];
                NAME = NAME + converter99.str();
                if(SCHMIN[ISCH]<10){
                    NAME = NAME + '0';
                }
                converter100<< dec  << SCHMIN[ISCH];
                NAME = NAME + converter100.str();
                if(SECSCH<10){
                    NAME = NAME + '0';
                }
                converter101 << dec << lround(SECSCH);
                NAME = NAME + converter101.str();

                converter98 << NAME;

                converter98.fill(' ');
                converter98 << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                converter98 << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                converter98 << setw(6)  << dec              << lround(SRSV)        << " ";
                converter98 << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                converter98 << setw(6)  << dec              << lround(SRGPS)       << " ";

                converter98 << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                converter98 << setw(4)  << dec              << lround(MDTR)        << " ";

                if(abs(SMDT) >= 999){
                    converter98 << setw(4)<<right << "****"<< " ";
                }
                else{
                    converter98 << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }
                converter98 << setw(4)  << dec              << lround(MDIO)        << " ";
                converter98 << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                converter98 << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                converter98 << setw(4) << lround(SMDI)        << " ";
                converter98 << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " ";
                abcd.clear();
                abcd = converter98.str();
                abcd.copy(CONVER_CK,abcd.length());
                CK = 0;
                for(i=0;i<125;i++){
                    CK = CK + CONVER_CK[i];
                }
                CK = (CK % 256);

                LUOUT <<NAME;

                LUOUT.fill(' ');
                LUOUT << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                LUOUT << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                LUOUT << setw(6)  << dec              << lround(SRSV)        << " ";
                LUOUT << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                LUOUT << setw(6)  << dec              << lround(SRGPS)       << " ";
                LUOUT << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                if(abs(MDTR) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDTR)        << " ";
                }

                if(abs(SMDT) > 999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDIO)        << " ";
                }

                if(abs(SMDI) > 999){
                     LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                }


                LUOUT << setw(4) << lround(SMDI)        << " ";
                LUOUT << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " " << setw(2) << hex <<uppercase<< CK << " " << FRC1[0] << FRC1[1] << FRC1[2] << " " << FRC2[0] << FRC2[1] << FRC2[2] << " "<<endl;

            };
        }
        else if(CODE == 'C'){

            if((TRKL[ISAT] == (MINUTOS_VETOR * 2)) && ((SATN[ISAT] < 300)&&(SATN[ISAT] > 200))){

                jkl = 0;

                while(jkl < 3000){

                    //0,08333 = 2/24

                    if((PPRN[jkl] == (SATN[ISAT]-200)) && (SCHTIME[ISCH] <= DATENAV[jkl]) && ((DATENAV[jkl] - SCHTIME[ISCH]) <= (0.08333))){
                        DJUL = DATENAV[jkl] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TOC = (DJUL * 86400);

                        goto pulo_BEIDOU;

                    };
                    jkl++;
                };
                if(jkl == 3000){

                    goto outravez;
                };

                // reject unhealthy satellites
                pulo_BEIDOU:
                if(BRORB[jkl][5][1] != 0){

                    goto outravez;
                };

                for(KSEC = 0; KSEC < (MINUTOS_VETOR * 2); KSEC++){

                    if(RESQUAD[KSEC][ISAT] != 0){
                        iii = 0;
                        CORPREV = 0;
                        DJUL = EPOCH[KSEC] - 44244;
                        DJUL = fmod(DJUL , 7);
                        TRC = DJUL * 86400;
                        IOE = BRORB[jkl][0][0];

                        pont_iii_BEIDOU:

                        if(iii == 0){
                            TAU = ((RESQUAD[KSEC][ISAT]) / VITLUM);
                        };
                        if(iii == 1){
                            TAU = (CORRGEOM / VITLUM);
                        };

                        aux5 = llround(TRC);
                        TTR = aux5 - TAU;

                        satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                        ALPHA = TAU * WE;

                        XS[0] = (X[0] * cos(ALPHA)) + (X[1] * sin(ALPHA));
                        XS[1] = (-X[0] * sin(ALPHA)) + (X[1] * cos(ALPHA));
                        XS[2] = X[2];

                        CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2]) * (ANTPOS[2] - XS[2])));

                        auxiliar = fabs(CORRGEOM - CORPREV);

                        if(auxiliar >= 0.00001){
                            CORPREV = CORRGEOM;
                            iii = 1;
                            goto pont_iii_BEIDOU;
                        };


                        IONO[KSEC] = MEASIONO[KSEC][ISAT];

                        azel(XS, ANTPOS, AZ, EL);


                        xyzlla(ANTPOS, LLA2);

                        MF = 1.0/(sin(EL) + (0.00143/ (tan(EL) + 0.0455)));


                        if(LLA2[2] <= 1000){
                            ZPD = 2162.0 + (NS * (1.0 - (LLA2[2] / 1000.0))) + (0.5 * DeltaN * (1.0 - (LLA2[2]/1000.0) * (LLA2[2]/1000.0)));
                        }
                        else{
                            ZPD = 732.0 - 8.0 * (NS * DeltaN)/ NSLOG * (exp(-NSLOG) - exp(0.125*(1.0 - (LLA2[2])/1000.0) * NSLOG));
                        };
                        TROPO[KSEC] = MF * ZPD/1000;

                        PCOR = RESQUAD[KSEC][ISAT] + (TREL * VITLUM) - TROPO[KSEC];

                        CR = PCOR - CORRGEOM;



                        if(OFSET == 0){
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL;

                        }
                        else{
                            auxiliar = pow(10,10);
                            REFSV[KSEC] = ((CR/VITLUM) * auxiliar) - CABDEL + REFDEL + (DELTAT[KSEC] * auxiliar);

                        };

                        TCLOCK = TTR - TOC;

                        if(TCLOCK > 302400){
                            TCLOCK = TCLOCK - 604800;
                        };
                        if(TCLOCK < -302400){
                            TCLOCK = TCLOCK + 604800;
                        };

                        auxiliar = pow(10,10);
                        CLOCKSAT = (SVCLK[jkl][0] + (SVCLK[jkl][1] * TCLOCK) + (SVCLK[jkl][2] * TCLOCK * TCLOCK)) * auxiliar;

                        //  computation of  REFGPS (unit= 0.1 ns)
                        REFGPS[KSEC] = REFSV[KSEC] + CLOCKSAT;

                    }
                    else{

                        REFGPS[KSEC] = 0;
                        REFSV[KSEC] = 0;
                        TROPO[KSEC] = 0;
                        IONO[KSEC] = 0;

                    };

                };

                TGD = BRORB[jkl][5][2];

                // AZth et ELv at the midpoint of the track

                CORPREV = 0;
                iii = 0;
                //0.004513889 = 6.5/24/60
                auxiliar = LEAPSEC / 86400.0;
                DJUL = SCHTIME[ISCH] - (ILEAPS/86400) + (((MINUTOS_VETOR/2.0)/24.0)/60.0) - 44244.0 + (auxiliar);


                DJUL = fmod(DJUL , 7);

                TRC = llround(DJUL * 86400.0);


                LLL_BEIDOU:
                if(iii == 0){
                    TAU = RESQUAD[12][ISAT] / VITLUM;
                };

                if(iii == 1){
                    TAU = CORRGEOM / VITLUM;
                };
                aux5 = llround(TRC);
                TTR = aux5  - TAU;


                satpos(BRORB, TTR, TREL, X, stat, jkl, SATNUM[ISAT], CODE);

                ALPHA =  TAU * WE;
                XS[0] = X[0] * cos(ALPHA) + X[1] * sin(ALPHA);
                XS[1] = - X[0] * sin(ALPHA) + X[1] * cos(ALPHA);
                XS[2] = X[2];
                CORRGEOM = sqrt(((ANTPOS[0] - XS[0]) * (ANTPOS[0] - XS[0])) + ((ANTPOS[1] - XS[1]) * (ANTPOS[1] - XS[1])) + ((ANTPOS[2] - XS[2])*(ANTPOS[2] - XS[2])) );

                auxiliar = fabs(CORRGEOM - CORPREV);

                if(auxiliar >= 0.00001){

                    CORPREV = CORRGEOM;
                    iii = 1;
                    goto LLL_BEIDOU;
                };

               azel(XS, ANTPOS, AZ, EL);

                EL = (EL/pi *1800);
                AZ = (AZ/pi *1800);

                ELV = llround(EL);
                AZTH = llround(AZ);


                fitlin (EPOCH, REFSV, REFSVMIL, SRSV, STDV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, REFGPS, REFGPSMIL, SRGPS, DSG, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, TROPO, MDTR, SMDT, SDTV, LEAPSEC, MINUTOS_VETOR);
                fitlin(EPOCH, IONO, MDIO, SMDI, ISG, LEAPSEC, MINUTOS_VETOR);

                // iono and tropo in 0.1 ns
                auxiliar = pow(10,10);
                MDTR = MDTR / VITLUM * auxiliar;
                MDIO = (MDIO/ VITLUM - TGD)* auxiliar;
                ISG = ISG / VITLUM * auxiliar;
                SRSV = SRSV * 1000 / 86400;
                SRGPS = SRGPS * 1000 / 86400;
                SMDT = SMDT / VITLUM * auxiliar * 1000 / 86400;
                SMDI = SMDI / VITLUM * auxiliar * 1000 / 86400;

                CLASS[0] = 'F';
                CLASS[1] = 'F';
                FRC1[0] = FF1[0][ISAT];
                FRC1[1] = FF1[1][ISAT];
                FRC1[2] = FF1[2][ISAT];
                FRC2[0] = FF2[0][ISAT];
                FRC2[1] = FF2[1][ISAT];
                FRC2[2] = FF2[2][ISAT];

                TRKL[ISAT] = TRKL[ISAT] * 30;

                SECSCH = 0;
                MJDTRK = MJD;
                ICK = 0;
                if((SCHMIN[ISCH] > (61 - MINUTOS_VETOR)) && (SCHH[ISCH] > 23)){
                    MJDTRK = MJD - 1;

                }

                ostringstream converter99,converter100,converter101,converter98;
                char CONVER_CK[135];
                string abcd;

                converter98.str("");
                converter98 << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;
                LUOUT << setw(3) << dec << SATN[ISAT] << " " << setw(1) << CLASS[0] << CLASS[1] << " " << setw(5) << dec << lround(MJDTRK) << " " ;

                NAME.clear();

                if(SCHH[ISCH]<10){
                    NAME = '0';
                }
                converter99<< dec  << SCHH[ISCH];
                NAME = NAME + converter99.str();
                if(SCHMIN[ISCH]<10){
                    NAME = NAME + '0';
                }
                converter100<< dec  << SCHMIN[ISCH];
                NAME = NAME + converter100.str();
                if(SECSCH<10){
                    NAME = NAME + '0';
                }
                converter101 << dec << lround(SECSCH);
                NAME = NAME + converter101.str();

                converter98 << NAME;

                converter98.fill(' ');
                converter98 << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                converter98 << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                converter98 << setw(6)  << dec              << lround(SRSV)        << " ";
                converter98 << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                converter98 << setw(6)  << dec              << lround(SRGPS)       << " ";

                converter98 << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                converter98 << setw(4)  << dec              << lround(MDTR)        << " ";

                if(abs(SMDT) >= 999){
                    converter98 << setw(4)<<right << "****"<< " ";
                }
                else{
                    converter98 << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }
                converter98 << setw(4)  << dec              << lround(MDIO)        << " ";
                converter98 << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                converter98 << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                converter98 << setw(4) << lround(SMDI)        << " ";
                converter98 << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " ";
                abcd.clear();
                abcd = converter98.str();
                abcd.copy(CONVER_CK,abcd.length());
                CK = 0;
                for(i=0;i<125;i++){
                    CK = CK + CONVER_CK[i];
                }
                CK = (CK % 256);

                LUOUT <<NAME;

                LUOUT.fill(' ');
                LUOUT << noshowpos<< " " << setw(4)<< dec << TRKL[ISAT] << " " << setw(3)<< dec << ELV << " " << setw(4)<< dec << AZTH << " ";
                LUOUT << setw(11) << dec <<showpos<< lround(REFSVMIL)       << " ";
                LUOUT << setw(6)  << dec              << lround(SRSV)        << " ";
                LUOUT << setw(11) << dec             << lround(REFGPSMIL)   << " ";
                LUOUT << setw(6)  << dec              << lround(SRGPS)       << " ";
                LUOUT << setw(4)  << dec <<noshowpos  << lround(DSG)         << " " <<setw(3) << IOE << " ";
                if(abs(MDTR) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDTR)        << " ";
                }

                if(abs(SMDT) > 999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec << showpos   <<right<< lround(SMDT)        << noshowpos << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << dec              << lround(MDIO)        << " ";
                }

                if(abs(SMDI) > 999){
                     LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << showpos          <<right<< lround(SMDI)        << " ";
                }

                if(abs(MDIO) > 9999){
                    LUOUT << setw(4)<<right << "****"<< " ";
                }
                else{
                    LUOUT << setw(4)  << noshowpos        << lround(MDIO)        << " ";
                }


                LUOUT << setw(4) << lround(SMDI)        << " ";
                LUOUT << setw(3)  << noshowpos<< lround(ISG) << " " << setw(2) << FR << " " << setw(2) << HC << " " << setw(2) << FRC << " " << setw(2) << hex <<uppercase<< CK << " " << FRC1[0] << FRC1[1] << FRC1[2] << " " << FRC2[0] << FRC2[1] << FRC2[2] << " "<<endl;

            };
        }


        outravez:
        continue;
    }

again:
continue;

}


quasefinal:
cout<< " ";


cout << "Program executed successfully." << '\n';
LUPARA.close();
LUOUT.close();
LULOG.close();
LUINP.close();
LUFOBS.close();
LUFOBP.close();
LUNAV.close();
LUNAV2P.close();

return 0;


}
