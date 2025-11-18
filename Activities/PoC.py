# Import Libraries
import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
from PIL import Image

# Function of Loading Data
def load_data():
    #data = pd.read_csv('/Users/hwang-eunji/PoC_HGU/PoC/player_stats.csv') 
    data = pd.read_csv('player_stats.csv') 
    return data

# Function Main
def main():
    st.markdown("<h1 style='text-align: center;'>배구 데이터 분석 및 팬 라인업 생성 🏐</h1>", unsafe_allow_html=True) #Title of Page
    
    st.markdown("""
    <hr style="border: 2px solid #4CAF50; margin: 20px 0;">
""", unsafe_allow_html=True) # Divide Line
    
    st.markdown("안녕하세요! 환영합니다 🤗 배구 팬들을 위한 공간입니다~!") # Text of Introduction

    # Load Data
    data = load_data()
    
    # Setting the Side Bar
    st.sidebar.title("필터 옵션")
    selected_team = st.sidebar.selectbox("팀 선택", data['Team'].unique())
    filtered_data = data[data['Team'] == selected_team]


    # Search on the Player Stats
    st.subheader("선수 스탯 조회")
    player_name = st.selectbox("선수 선택하기", filtered_data['Name'].unique(), key="player_selectbox")
    player_stats = filtered_data[filtered_data['Name'] == player_name]
    st.write(player_stats)

     
    # Plotting the Stats 
    st.header("선수 통계 시각화")
    positions = filtered_data['Position'].unique()


    # Visualization
    for position in positions:
        st.subheader(f"{position} 포지션 선수 성과")
        position_data = filtered_data[filtered_data['Position'] == position].nlargest(20, 'Score')

        if position in ["OP", "OH"]: # Position of OP, OH
            fig, ax = plt.subplots(figsize=(20, 6))
            ax.bar(position_data['Name'], position_data['Attack'], color='skyblue', label='Attack')
            ax.bar(position_data['Name'], position_data['Score'], color='lightgreen', label='Score', alpha=0.7)
            ax.set_ylabel('Value')
            ax.set_title(f"{position} Position offense and scoring")
            ax.legend()
            st.pyplot(fig)

        elif position == "MB": # Position of MB
            fig, ax = plt.subplots(figsize=(20, 6))
            ax.bar(position_data['Name'], position_data['Blocking'], color='salmon', label='Blocking')
            ax.bar(position_data['Name'], position_data['Score'], color='lightgreen', label='Score', alpha=0.7)
            ax.set_ylabel('Value')
            ax.set_title(f'{position} Position blocking and scoring')
            ax.legend()
            st.pyplot(fig)

        elif position == "S": # Position of Setter
            fig, ax = plt.subplots(figsize=(20, 6))
            ax.bar(position_data['Name'], position_data['Sets'], color='orange', label='Sets')
            ax.bar(position_data['Name'], position_data['Score'], color='lightgreen', label='Score', alpha=0.7)
            ax.set_ylabel('Value')
            ax.set_title(f'{position} Position Sets and scoring')
            ax.legend()
            st.pyplot(fig)

        elif position == "L": # Position of Libero
            fig, ax = plt.subplots(figsize=(20, 6))
            ax.bar(position_data['Name'], position_data['Average per Set Dig'], color='orange', label='Dig rates')
            ax.bar(position_data['Name'], position_data['Score'], color='lightgreen', label='Score', alpha=0.7)
            ax.set_ylabel('Value')
            ax.set_title(f'{position} Position defense and scoring')
            ax.legend()
            st.pyplot(fig)

    st.markdown("""
    <hr style="border: 3px dashed #4682B4; margin: 20px 0;">
""", unsafe_allow_html=True) # Divide the line on the Page

    # Visualization of the score per each Teams
    st.header("팀별 총 점수")
    total_score = data.groupby("Team")["Score"].sum()
    fig3, ax3 = plt.subplots(figsize=(20, 8))
    ax3.bar(total_score.index, total_score.values, color='salmon')
    ax3.set_ylabel('Total Score')
    ax3.set_title('Total Score by Team')
    st.pyplot(fig3)

    st.markdown("""
        <hr style="border: 2px solid #6A5ACD; margin: 20px 0;">
""", unsafe_allow_html=True) # divied the page by line

    # Construct the starting members 
    st.header("선발 라인업 구성하기")
    
    selected_players = st.multiselect("포지션에 따라 선수 선택하기", data['Name'].unique())
    if st.button('선발 라인업 보기'):
        positions = ['OH', 'MB', 'S', 'OP', 'L']
        lineup = {pos: [] for pos in positions}
        
        for player in selected_players:
            player_data = data[data['Name'] == player].iloc[0]
            position = player_data['Position']
            lineup[position].append(player)

        # Display the players that client choosed
        for pos, players in lineup.items():
            if players:
                st.write(f"{pos} 포지션: {', '.join(players)}")


if __name__ == "__main__":
    main()


